(ns feedcircuit-revisited.backend
  (:require [feedcircuit-revisited.conf :as conf]
            [clojure.string :as cstr]))

(require '[feedcircuit-revisited.fs-backend :as b])

(def get-feed-attrs b/get-feed-attrs)

(def add-feed! b/add-feed!)

(def update-feed! b/update-feed!)

(def append-items! b/append-items!)

(def known-ids b/known-ids)


(def get-items b/get-items)

(def get-items-backwards b/get-items-backwards)

(def get-item b/get-item)

(def add-content! b/add-content!)

(def item-id? b/item-id?)

(def all-users b/all-users)

(def get-user-data b/get-user-data)

(def selected-add! b/selected-add!)

(def selected-remove! b/selected-remove!)

(def update-settings! b/update-settings!)

(def update-positions! b/update-positions!)

(def active-feed-urls b/active-feed-urls)

(def unknown-feeds b/unknown-feeds)

(defn get-feed-items [feed start]
  (let [feed-title (:feed/title (get-feed-attrs feed))]
    (->> (get-items-backwards feed start)
         (map #(assoc % :feed/title feed-title)))))


(defn parse-filters
  "Filter expression consists of a list of
   authors and categories to include or exclude."
  [filters]
  (let [parse-filter #(let [include (not= \! (first %))]
                      [(if include % (apply str (rest %)))
                       include])
        add-default #(if (some second %) % (conj % [:all true]))]
    (->> (if (cstr/blank? filters) [] (cstr/split filters #","))
         (map cstr/trim)
         (map cstr/lower-case)
         (map parse-filter)
         vec
         add-default)))

(defn get-attrs-for-filter [item]
  (->> (concat (:item/author item) (:item/category item))
       (map cstr/lower-case)
       vec))

(defn item-matches [item expressions]
  (let [attrs (get-attrs-for-filter item)]
    (if (empty? attrs)
      true
      (first (for [[term verdict] expressions
                   attr attrs
                   :when (or (= term :all)
                             (= attr term))]
               verdict)))))

(defn get-unread-items [sources]
  (apply concat
         (for [{feed :source/feed
                filters :source/filters
                pos :source/position
                feed-title :feed/title} (filter :source/active sources)
               :let [exprs (parse-filters filters)]]
           (->> (get-items feed pos)
                (filter #(item-matches % exprs))
                (map #(assoc % :feed/title feed-title))))))

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

(def init! b/init!)
