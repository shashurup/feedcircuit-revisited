(ns feedcircuit-revisited.backend
  (:require [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.storage :as storage]
            [clojure.set :as cset]
            [clojure.string :as cstr]
            [clojure.tools.logging :as log]
            [me.raynes.fs :as fs]))

(defonce feed-index (agent {}
                           :error-handler #(log/error % "Failed to update feed index")))

(defn get-dir [feed]
  (get-in @feed-index [feed :dir]))

(defn get-feed-attrs [feed]
  (storage/get-attrs (get-dir feed)))

(defn set-feed-attrs [feed attrs]
  (storage/set-attrs (get-dir feed) attrs))

(defn all-feeds [] @feed-index)

(defn get-unique-id [item]
  (if-let [num (:num item)]
    (str num "," (:feed item))
    (:link item)))

(defn as-int [subj]
  (try
    (Long/parseLong subj)
    (catch NumberFormatException _ nil)))

(defn add-uid-and-feed-title [item]
  (assoc item
         :uid (get-unique-id item)
         :feed-title (:title (get-feed-attrs (:feed item)))))

(defn init-feed-index! [_ data-dir]
  (->> (fs/list-dir (str data-dir "/feeds"))
       (map fs/normalized)
       (filter fs/directory?)
       (map #(vector (:url (storage/get-attrs %))
                     {:dir (str %)}))
       (into {})))

(defn load-feed! [index url]
  (when-not (get-in index [url :item-count])
    (let [items (storage/get-items (get-in index [url :dir]) 0)]
      (update index url merge {:item-count (count items)
                               :known-ids (set (map :id items))}))))

(defn get-item-count [feed]
  (if-let [result (get-in @feed-index [feed :item-count])]
    result
    (do
      (send feed-index load-feed! feed)
      (await feed-index)
      (get-item-count feed))))

(defn add-feed-num-uid [item feed num]
  (let [item (assoc item :feed feed :num num)]
    (assoc item :uid (get-unique-id item))))

(defn get-items 
  "Returns lazy numbered sequence of items
   in the directory dir beginning from the start"
  [feed start]
  (let [dir (get-dir feed)]
    (map-indexed #(add-feed-num-uid %2 feed (+ start %1))
                 (storage/get-items dir start))))

(defn get-items-backwards
  "Returns lazy numbered sequence of items
   in the directory dir beginning from the start
   and moving backwards"
  ([feed]
   (get-items-backwards feed (dec (get-item-count feed))))
  ([feed start]
   (let [dir (get-dir feed)
         start (or start (dec (get-item-count feed)))]
     (map-indexed #(add-feed-num-uid %2 feed (- start %1))
                  (storage/get-items-backwards dir start)))))

(defn parse-item-id [subj]
  (if (string? subj)
    (when-let [[_ ord-num feed] (re-matches #"([0-9]+),(.*)" subj)]
      [feed (as-int ord-num)])))

(defn get-item [uid]
  (when-let [[feed pos] (parse-item-id uid)]
    (first (get-items feed pos))))

(defn get-feed-items [feed start]
  (let [feed-title (:title (get-feed-attrs feed))]
    (->> (get-items-backwards feed start)
         (map #(assoc % :feed-title feed-title)))))

(def item-id? parse-item-id)

;=============================

(defn dir-name [url]
  (-> url
      (cstr/replace "http://" "")
      (cstr/replace "https://" "")
      (cstr/replace "/" ".")))

(defn dir-path [url]
  (str (fs/normalized (str (conf/param :data-dir) "/feeds/" (dir-name url)))))

(defn get-known-ids [feed ids]
  (get-in @feed-index [feed :known-ids]))

(defn add-feed! [feed attrs]
  (send feed-index
        (fn [index]
          (when-not (index feed)
            (let [dir (dir-path feed)]
              (fs/mkdirs dir)
              (storage/set-attrs dir (assoc attrs :url feed))
              (assoc index feed {:dir dir
                                 :item-count 0
                                 :known-ids #{}})))))
  (await feed-index))

(defn update-feed! [feed attrs]
  (send feed-index
        (fn [index]
          (let [dir (get-in index [feed :dir])]
            (storage/set-attrs dir (merge (storage/get-attrs dir) attrs))
            index))))

(defn append-items! [feed items]
  (send feed-index
        (fn [index]
          (let [cnt (get-in index [feed :item-count])
                known-ids (get-in index [feed :known-ids])
                new-items (remove #(known-ids (:id %)) items)
                dir (get-in index [feed :dir])
                new-item-count (count new-items)]
            (storage/append-items! dir new-items)
            (update index feed merge {:item-count (+ cnt new-item-count)
                                      :last-sync-count new-item-count
                                      :known-ids (cset/union known-ids
                                                             (set (map #(:id %) new-items)))}))))
  (await feed-index)
  (get-in @feed-index [feed :last-sync-count]))


; === user handling ===

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
  (->> (concat (:author item) (:category item))
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
         (for [{feed :feed
                filters :filters
                pos :position} (filter :active sources)
               :let [exprs (parse-filters filters)
                     feed-title (:title (get-feed-attrs feed))]]
           (->> (get-items feed pos)
                (filter #(item-matches % exprs))
                (map #(assoc % :feed-title feed-title))))))

(defn all-users []
  (->> (str (conf/param :data-dir) "/users")
       fs/list-dir
       (map fs/base-name)))

(defn user-dir [id]
  (str (conf/param :data-dir) "/users/" id))

(defonce user-attrs (atom {}))

(defonce attrs-updater (agent nil))

(defn write-user-attrs [attrs]
  (let [dir (user-dir (:id attrs))]
    (fs/mkdirs dir)
    (storage/set-attrs dir
                       (-> attrs
                           (dissoc :id)
                           (update :unread vec)))))

(defn attr-update-watcher [_ _ _ attrs]
  (send attrs-updater (fn [_] (write-user-attrs attrs))))

(defn read-user-attrs [user-id]
  (-> (storage/get-attrs (user-dir user-id))
      (assoc :id user-id)
      (update :unread #(apply sorted-set %))))

(defn ensure-user-attrs [user-id]
  (let [mk-atom #(add-watch (atom %) user-id attr-update-watcher)
        add-user #(if (% user-id)
                    %
                    (assoc % user-id (mk-atom (read-user-attrs user-id))))]
    (or (@user-attrs user-id)
        ((swap! user-attrs add-user) user-id))))

(defn get-user-attrs [user-id]
  (deref (ensure-user-attrs user-id)))

(defn update-user-attrs! [user-id f & args]
  (apply swap! (ensure-user-attrs user-id) f args))

(defn cache-item-content [item]
  (when-not (:content item)
    (let [feed (:feed item)
          content-ident (when feed
                          (:content-ident (get-feed-attrs feed)))]
      (future (content/cache-content! (:link item)
                                      content-ident)))))

(defn selected-add! [user-id ids]
  (let [items (map #(if (item-id? %) (get-item %) %) ids)]
    (doall (map cache-item-content items))
    (update-user-attrs! user-id update :selected into items)))

(defn selected-remove! [user-id ids]
  (letfn [(in-ids? [item]
            (let [id (get-unique-id item)]
              (some #(= id %) ids)))]
    (update-user-attrs! user-id update :selected #(remove in-ids? %))))

(defn get-user-data [user-id]
  (let [{id :id
         feeds :feeds
         positions :positions
         selected :selected
         styles :styles} (get-user-attrs user-id)]
    {:id id
     :sources (map-indexed (fn [idx feed]
                             (let [[url filters] (cstr/split feed #" " 2)
                                   active (not= (first url) \#)
                                   url (if active url (subs url 1))]
                               {:num idx
                                :active active
                                :id url
                                :feed url
                                :filters filters
                                :position (get positions url)}))
                           feeds)
     :selected (map add-uid-and-feed-title selected)
     :styles styles}))

(defn get-selected-items
  "Lazy sequence of items user marked for later reading."
  [user-id]
  (let [{sources :sources
         selected :selected} (get-user-data user-id)
        nums (into {} (map #(vector (:id %) (:num %)) sources))]
    (sort-by #(vector (nums (:feed %)) (:num %)) selected)))

(defn initial-position [feed]
  (or (:num (last (take 16 (get-items-backwards feed)))) 0))

(defn update-settings! [user-id sources styles]
  (let [positions (:positions (get-user-attrs user-id))
        feeds (map #(str (when-not (:active %) "#")
                         (:feed %)
                         (when (:filters %) " ")
                         (:filters %))
                   sources)
        missing-positions (->> sources
                               (filter :active)
                               (map :feed)
                               (remove positions)
                               (map #(vector % (initial-position %)))
                               (into {}))]
    (update-user-attrs! user-id assoc :feeds (vec feeds)
                                      :styles styles
                                      :positions (merge positions missing-positions))))

(defn add-source! [user-id url]
  (let [{sources :sources
         styles :styles} (get-user-data user-id)]
    (update-settings! user-id
                      (conj sources {:active true :feed url})
                      styles)))

(defn update-positions! [user-id positions]
  (update-user-attrs! user-id update :positions merge positions))

(defn init! []
  (send feed-index init-feed-index! (conf/param :data-dir))
  (await feed-index))
