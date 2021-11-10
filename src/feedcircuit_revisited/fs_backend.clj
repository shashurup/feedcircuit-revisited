(ns feedcircuit-revisited.fs-backend
  (:require [feedcircuit-revisited.conf :as conf]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [clojure.string :as cstr]
            [clojure.tools.logging :as log]
            [me.raynes.fs :as fs]))

(defn as-int [subj]
  (try
    (Long/parseLong subj)
    (catch NumberFormatException _ nil)))

; === storage ===

(defn write-file [filename data]
  (let [tempfilename (str filename ".temp")]
    (with-open [w (io/writer tempfilename)]
      (binding [*out* w] (pr data)))
    (fs/rename tempfilename filename)))

(defn read-file [filename]
  (if (fs/exists? filename)
    (with-open [r (java.io.PushbackReader. (io/reader filename))]
      (edn/read r))))

; a good place to add cache
(def get-data read-file)

(defn set-data [filename data]
  (write-file filename data)
  data)

(defn get-block [dir block-num]
  (get-data (str dir "/" block-num)))

(defn set-block [dir block-num data]
  (set-data (str dir "/" block-num) data))

(defn get-last-block-num [dir]
  (->> (fs/list-dir dir)
       (map fs/base-name)
       (filter #(re-matches #"[0-9]+" %))
       (map as-int)
       (cons 0)  ; in case there are no blocks yet
       (apply max)))

(def ^:dynamic block-size "Number of items in each file" 100)

(defn read-items
  "Returns lazy sequence of items in the directory dir
   beginning from the start"
  [dir start]
  (let [start-block (quot start block-size)
        start-offset (rem start block-size)
        items (apply concat (take-while not-empty
                                        (map #(get-block dir %)
                                             (iterate inc start-block))))]
    (drop start-offset items)))


(defn read-items-backwards
  "Returns lazy sequence of items in the directory dir
   beginning from the start and moving backwards"
  [dir start]
  (let [start-block (quot start block-size)
        first-block (get-block dir start-block)
        start-offset (rem start block-size)]
    (apply concat
           (drop (- (count first-block) (inc start-offset))
                 (reverse first-block))
           (take-while not-empty
                       (map #(reverse (get-block dir %))
                            (iterate dec (dec start-block)))))))


(defn write-items!
  "Appends items to the end of the list in the directory dir"
  [dir items]
  (let [last-block-num (get-last-block-num dir)
        last-block (get-block dir last-block-num)
        new-blocks (->> (concat last-block items)
                        (partition-all block-size)
                        (map vec))
        start (+ (* last-block-num block-size)
                 (count last-block))]
    (doseq [[num block] (map-indexed vector new-blocks)]
      (set-block dir (+ last-block-num num) block))
    (range start (+ start (count items)))))

(defn get-attrs [dir]
  (get-data (str dir "/attrs")))

(defn set-attrs [dir attrs]
  (set-data (str dir "/attrs") attrs))

; === feed handling ===

(defonce feed-index (agent {}
                           :error-handler #(log/error % "Failed to update feed index")))

(defn get-dir [feed]
  (get-in @feed-index [feed :dir]))

(defn ensure-ns [subj ns]
  (into {} (for [[k v] subj]
             [(if (namespace k)
                k
                (keyword ns (name k))) v])))

(defn get-feed-attrs [feed]
  (ensure-ns (get-attrs (get-dir feed)) "feed"))

(defn all-feeds [] @feed-index)

(defn get-unique-id [item]
  (if-let [num (:item/num item)]
    (str num "," (:item/feed item))
    (:item/link item)))

(defn add-uid [item]
  (-> item
      (assoc :item/source-id (:item/id item))
      (assoc :item/id (get-unique-id item))))

(defn add-uid-and-feed-title [item]
  (-> item
      (ensure-ns "item")
      add-uid
      (assoc :feed/title (:feed/title (get-feed-attrs (:feed item))))))

(defn init-feed-index! [_ data-dir]
  (->> (fs/list-dir (str data-dir "/feeds"))
       (map fs/normalized)
       (filter fs/directory?)
       (map #(vector (:url (get-attrs %))
                     {:dir (str %)}))
       (into {})))

(defn load-feed! [index url]
  (when-not (get-in index [url :item-count])
    (let [items (read-items (get-in index [url :dir]) 0)]
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
  (let [item (assoc item :item/feed feed :item/num num)]
    (add-uid (ensure-ns item "item"))))

(defn get-items 
  "Returns lazy numbered sequence of items
   in the directory dir beginning from the start"
  [feed start]
  (let [dir (get-dir feed)]
    (map-indexed #(add-feed-num-uid %2 feed (+ start %1))
                 (read-items dir start))))

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
                  (read-items-backwards dir start)))))


; === content caching ===


(defonce content-index (agent {} :error-handler #(log/error % "Failed to update content index")))

(defn content-dir [] (str (conf/param :data-dir) "/content"))

(defn init-content-index! [_]
  (into {}
        (map-indexed #(vector (first %2) %1)
                    (read-items (content-dir) 0))))

(defn enrich-with-content [item]
  (if (:item/content item)
    item
    (if-let [idx (@content-index (:item/link item))]
      (binding [block-size 8]
        (let [[_ _ content] (first (read-items (content-dir) idx))]
          (assoc item :item/content content)))
      item)))

(defn parse-item-id [subj]
  (if (string? subj)
    (when-let [[_ ord-num feed] (re-matches #"([0-9]+),(.*)" subj)]
      [feed (as-int ord-num)])))

(defn get-item [uid]
  (when-let [[feed pos] (parse-item-id uid)]
    (enrich-with-content (first (get-items feed pos)))))

(defn add-content!
  ([uid content]
   (if-let [{url :item/link
             title :item/title} (get-item uid)]
     (add-content! url title content)))
  ([url title content]
   (letfn [(add! [index]
             (if (index url)
               index
               (let [dir (content-dir)]
                 (fs/mkdirs dir)
                 (binding [block-size 8]
                   (assoc index
                          url
                          (last (write-items! dir [[url title content]])))))))]
     (send content-index add!))))

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
              (set-attrs dir (assoc attrs :url feed))
              (assoc index feed {:dir dir
                                 :item-count 0
                                 :known-ids #{}})))))
  (await feed-index))

(defn update-feed! [feed attrs]
  (send feed-index
        (fn [index]
          (let [dir (get-in index [feed :dir])]
            (set-attrs dir (merge (get-attrs dir) attrs))
            index))))

(defn append-items! [feed items]
  (send feed-index
        (fn [index]
          (let [cnt (get-in index [feed :item-count])
                known-ids (get-in index [feed :known-ids])
                new-items (remove #(known-ids (:id %)) items)
                dir (get-in index [feed :dir])
                new-item-count (count new-items)]
            (write-items! dir new-items)
            (update index feed merge {:item-count (+ cnt new-item-count)
                                      :last-sync-count new-item-count
                                      :known-ids (cset/union known-ids
                                                             (set (map #(:id %) new-items)))}))))
  (await feed-index)
  (get-in @feed-index [feed :last-sync-count]))


; === user handling ===

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
    (set-attrs dir
               (-> attrs
                   (dissoc :id)
                   (update :unread vec)))))

(defn attr-update-watcher [_ _ _ attrs]
  (send attrs-updater (fn [_] (write-user-attrs attrs))))

(defn read-user-attrs [user-id]
  (-> (get-attrs (user-dir user-id))
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

(defn separate-content [item]
  (if (:item/content item)
    (let [{url :item/link
           title :item/title
           content :item/content} item]
      (add-content! url title content)
      (dissoc item :item/content))
    item))

(defn selected-add! [user-id ids]
  (let [items (map #(if (item-id? %)
                      (get-item %)
                      (separate-content %)) ids)]
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
    {:user/id id
     :user/sources (map-indexed (fn [idx feed]
                             (let [[url filters] (cstr/split feed #" " 2)
                                   active (not= (first url) \#)
                                   url (if active url (subs url 1))]
                               {:source/num idx
                                :source/active active
                                :source/id url
                                :source/feed url
                                :source/filters filters
                                :source/position (get positions url)}))
                           feeds)
     :user/selected (map add-uid-and-feed-title selected)
     :user/styles styles}))

(defn initial-position [feed]
  (or (:num (last (take 16 (get-items-backwards feed)))) 0))

(defn update-settings! [user-id sources styles]
  (let [positions (:positions (get-user-attrs user-id))
        feeds (map #(str (when-not (:source/active %) "#")
                         (:source/feed %)
                         (when (:source/filters %) " ")
                         (:source/filters %))
                   sources)
        missing-positions (->> sources
                               (filter :source/active)
                               (map :source/feed)
                               (remove positions)
                               (map #(vector % (initial-position %)))
                               (into {}))]
    (update-user-attrs! user-id assoc :feeds (vec feeds)
                                      :styles styles
                                      :positions (merge positions missing-positions))))

(defn update-positions! [user-id positions]
  (update-user-attrs! user-id update :positions merge positions))

(defn init! []
  (send feed-index init-feed-index! (conf/param :data-dir))
  (send content-index init-content-index!)
  (await feed-index))
