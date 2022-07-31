(ns feedcircuit-revisited.backend
  (:require [feedcircuit-revisited.conf :as conf]
            [clojure.string :as cstr]
            [feedcircuit-revisited.datomic-backend :as d-back]
            [feedcircuit-revisited.fs-backend :as fs-back]))

; === backend switching machinery ===

(defn stub [& args]
  (throw (Exception. "Not implemented")))

(def backend-interface ['get-feed-attr 'get-feed-attr-by-id
                        'add-feed! 'update-feed! 'append-items!
                        'active-feed-urls 'unknown-feeds
                        'get-items 'get-items-backwards 'get-item 'known-ids
                        'add-content! 'item-id? 'get-user-data
                        'selected-add! 'selected-remove!
                        'update-settings! 'update-positions!
                        'init-impl!])

(defonce _ (doseq [sym backend-interface]
             (intern *ns* sym stub)))

(defn map-backend-impl [impl]
  (let [impl-ns (ns-interns impl)]
    (doseq [sym backend-interface]
      (let [bfn (get impl-ns sym stub)]
        (intern 'feedcircuit-revisited.backend sym bfn)))))

; === common backend functions ===

(defn get-feed-items [feed start]
  (let [feed-title (get-feed-attr-by-id feed :feed/title)]
    (->> (get-items-backwards feed start)
         (map #(assoc % :feed/title feed-title)))))

(defn unescape [subj]
  (cstr/replace subj
                #"%[0-F][0-F]"
                #(-> %
                     (subs 1)
                     (Integer/parseInt 16)
                     char
                     str)))

(defn parse-filter [subj]
  (let [include (not= \! (first subj))
        subj (if include subj (subs subj 1))
        subj (if (re-matches #"^/.*/$" subj)
               (re-pattern (unescape (subs subj 1 (dec (count subj)))))
               (cstr/lower-case (unescape subj)))]
    [subj include]))

(defn parse-filters
  "Filter expression consists of a list of
   authors and categories to include or exclude."
  [filters]
  (let [add-default #(if (some second %) % (conj % [:all true]))]
    (->> (if (cstr/blank? filters) [] (cstr/split filters #","))
         (map (comp parse-filter cstr/trim))
         vec
         add-default)))

(defn get-attrs-for-filter [item]
  (->> (concat (:item/author item)
               (:item/category item))
       (map cstr/lower-case)
       (cons [:title (:item/title item)])
       vec))

(defn regex? [subj] (instance? java.util.regex.Pattern subj))

(defmulti attr-matches  #(cond (= %1 :all) :all
                               (and (regex? %1)
                                    (vector %2)
                                    (= (first %2) :title)) :title
                               :else :attr))

(defmethod attr-matches :all [_ _] true)

(defmethod attr-matches :title [pattern [_ title]] (re-find pattern title))

(defmethod attr-matches :attr [term attr] (= term attr))

(defn item-matches [item expressions]
  (let [attrs (get-attrs-for-filter item)]
    (first (for [[term verdict] expressions
                 attr attrs
                 :when (attr-matches term attr)]
             verdict))))

(defn get-unread-items [sources]
  (apply concat
         (for [{feed :source/feed
                src :source/id
                filters :source/filters
                pos :source/position
                feed-title :feed/title} (->> sources
                                             (filter :source/active)
                                             (remove :source/seen))
               :let [exprs (parse-filters filters)]]
           ;; lazy-seq is essential here, it prevents concat
           ;; from prematurely consuming items from get-items
           ;; thus causing reading items from disk
           (->> (lazy-seq (get-items feed pos))
                (filter #(item-matches % exprs))
                (map #(assoc % :feed/title feed-title
                               :item/source src))))))

(defn get-selected-items
  "Lazy sequence of items user marked for later reading."
  [user-id]
  (let [{sources :user/sources
         selected :user/selected} (get-user-data user-id :selected/details)
        nums (into {} (map #(vector (:source/feed %) (:source/num %)) sources))]
    (sort-by #(vector (nums (:item/feed %)) (:item/num %)) selected)))

(defn add-source! [user-id url]
  (let [{sources :user/sources
         styles :user/styles} (get-user-data user-id)]
    (update-settings! user-id
                      (conj sources {:source/active true :source/feed url})
                      styles)))

(defn init! []
  (if (conf/param :datomic)
    (map-backend-impl 'feedcircuit-revisited.datomic-backend)
    (map-backend-impl 'feedcircuit-revisited.fs-backend))
  (init-impl!))
