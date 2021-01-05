(ns feedcircuit-revisited.feed
  (:require [clojure.xml :as xml]
            [clojure.set :as cset]
            [clojure.string :as cstr]
            [clojure.edn :as edn]
            [java-time :as jt]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [feedcircuit-revisited.storage :as storage]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.rfc822 :as rfc822]
            [clojure.core.memoize :as memz]
            [clojure.string :as s]
            [clojure.tools.logging :as log]))

; === feed parsing ===

(def attr-map {:guid :id
               :pubDate :published
               :updated :published
               :description :summary})

(def array-attrs #{:author :category :contributor})

(defn content-str [attr] (apply str (:content attr)))

(defn get-link-url [attr]
  (if-let [link (get-in attr [:attrs :href])]
    (if (= "alternate" (get-in attr [:attrs :rel] "alternate"))
      link)
    (content-str attr)))

(defn from-rfc1123-datetime [attr]
  (rfc822/parse-datetime (content-str attr)))

(defn try-parse-something-else [subj]
  ;; TODO make parsing more robust including other datetime formats
  )

(defn parse-rss-datetime [attr]
  (let [dt (cstr/trim (content-str attr))]
    (try
      (rfc822/parse-datetime dt)
      (catch Exception _ (try-parse-something-else dt)))))

(def attr-convert {:pubDate parse-rss-datetime
                   :link get-link-url})

(defn parse-rss-item-attribute [item attr]
  (let [tag (:tag attr)
        conv (get attr-convert tag content-str)
        upd-fn (if (contains? array-attrs tag) (fn [old new] (conj (or old []) new))
                                               (fn [old new] new))]
    (if-let [attr-val (conv attr)]
      (update-in item [(get attr-map tag tag)] upd-fn attr-val)
      item)))

(defn ensure-item-id [item]
  (assoc item :id (or (:id item)
                      (:link item)
                      (:title item)
                      (hash (:summary item)))))

(defn parse-rss-item [item]
  (ensure-item-id (reduce parse-rss-item-attribute {} (:content item))))

(defn find-channel [feed-xml]
  (first (filter #(= (:tag %) :channel)
                 (:content feed-xml))))

(defn extract-rss-items [feed-xml]
  (let [items (concat (:content (find-channel feed-xml))
                      (:content feed-xml))]
    (filter #(contains? #{:item :entry} (:tag %)) items)))

(defn find-details [feed-xml]
  (or (find-channel feed-xml) feed-xml))

(defn parse-feed-details [feed-xml]
  (->> (:content (find-details feed-xml))
       (filter #(not (contains? #{:item :entry} (:tag %))))
       (reduce parse-rss-item-attribute {})))

(defn fetch-items
  "Fetches new items from the feed located at the url."
  [url]
  (let [reply (http/get url (merge {:as :stream}
                                   content/http-timeouts))
        feed-xml (xml/parse (:body reply))
        attrs (parse-feed-details feed-xml)
        items (map parse-rss-item (extract-rss-items feed-xml))]
     [attrs items]))

; === feed handling ===

(defonce feed-index (agent {}
                           :error-handler #(log/error % "Failed to update feed index")))

(defn get-dir [feed]
  (get-in @feed-index [feed :dir]))

(defn get-internal-id [item]
  (if-let [num (:num item)]
    [(:feed item) num]
    (:link item)))

(defn get-numbered-items 
  "Returns lazy numbered sequence of items
   in the directory dir beginning from the start"
  [feed start]
  (let [dir (get-dir feed)]
    (map-indexed #(assoc %2 :num (+ start %1))
                 (storage/get-items dir start))))

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

;=============================

(defn dir-name [url]
  (-> url
      (cstr/replace "http://" "")
      (cstr/replace "https://" "")
      (cstr/replace "/" ".")))

(defn dir-path [url]
  (str (fs/normalized (str (conf/param :data-dir) "/feeds/" (dir-name url)))))

(defn fix-summary-and-content
  "Summary may be absent from item, in this case
   it is deduced from content. Summary may be too long,
   in this case it is made shorter. At the same time
   content may be absent, in this case summary takes its place."
  [item]
  (let [summary (:summary item)
        content (:content item)]
    (cond
      (and (empty? summary) (not (empty? content)))
        (assoc item :summary (content/summarize content))
      (and summary (> (content/calculate-size summary) 1024))
        (let [new-summary (content/summarize summary)]
          (if (> (- (count summary) (count new-summary)) 512)
            (assoc item :summary new-summary
                        :content (or content summary))
            item))
      :else item)))

(defn fix-refs
  "Make all references in content and summary absolute"
  [item base-url]
  (cond-> item
    (:summary item) (update :summary content/make-refs-absolute base-url)
    (:content item) (update :content content/make-refs-absolute base-url)))

(defn append-items! [index url attrs items]
  (let [cnt (get-in index [url :item-count])
        known-ids (get-in index [url :known-ids])
        dir (get-in index [url :dir])
        new-item-count (count items)]
    (storage/set-attrs dir (merge (storage/get-attrs dir)
                                  attrs))
    (storage/append-items! dir items)
    (update index url merge {:item-count (+ cnt new-item-count)
                             :last-sync-count new-item-count
                             :known-ids (cset/union known-ids
                                                    (set (map #(:id %) items)))})))

(defn apply-items! [index url attrs items]
  (let [known-ids (get-in index [url :known-ids])
        new-items (->> items
                       (remove #(known-ids (:id %)))
                       (map #(fix-refs % url))
                       (map fix-summary-and-content)
                       (sort-by :published))]
    (append-items! index url attrs new-items)))

(defn new-feed! [index url attrs items]
  (when-not (index url)
    (let [dir (dir-path url)]
      (fs/mkdirs dir)
      (apply-items! (assoc index url {:dir dir
                                      :item-count 0
                                      :known-ids #{}})
                    url
                    (assoc attrs :url url)
                    items))))

(defn add-feed! [url]
  (let [[attrs items] (fetch-items url)]
    (send feed-index new-feed! url attrs items)
    (await feed-index)))

(defn sync-feed! [url]
  (let [[attrs items] (fetch-items url)]
    (send feed-index apply-items! url attrs items)
    (await feed-index)
    (get-in @feed-index [url :last-sync-count])))

(defn sync-and-log-safe! [url]
  (log/info "Fetching" url)
  (try
    (let [result (sync-feed! url)]
      (log/info ">>" result "items from" url)
      result)
    (catch Exception ex
      (log/error ex "Failed to get news from" url))))

(defn next-update-time
  "Deduce next update time for the feed located at url.
   The algorithm takes into account how often the feed is updated."
  [url]
  (let [last-items (storage/get-items (get-dir url)
                                      (max 0 (- (get-item-count url) 10)))
        dates (->> last-items
                   (map :published)
                   (remove nil?)
                   (map #(jt/instant (jt/formatter :iso-date-time) %)))
        deltas (->> dates
                    (map jt/to-millis-from-epoch)
                    (partition 2 1)
                    (map (fn [[b e]] (- e b))))]
    (if (empty? deltas)
      (jt/instant 0)
      (jt/plus (apply jt/max dates)
               (jt/min (jt/hours 24)
                       (jt/millis (quot (apply + deltas)
                                        (count deltas))))))))

(defn get-feed-attrs [feed]
  (storage/get-attrs (get-dir feed)))

(defn set-feed-attrs [feed attrs]
  (storage/set-attrs (get-dir feed) attrs))

(defn get-feed-items [feed start]
  (->> (get-numbered-items feed start)
       (map #(assoc % :iid [feed (:num %)]))))

; === user handling ===

(defn parse-feed-expression
  "Feed expression consists of the feed url and a list of
   authors and categories to include or exclude."
  [expr]
  (let [parse-expr #(let [include (not= \! (first %))]
                      [(if include % (apply str (rest %)))
                       include])
        add-default #(if (some second %) % (conj % [:all true]))
        [url filters] (cstr/split expr #" " 2)]
    [url
     (->> (if (cstr/blank? filters) [] (cstr/split filters #","))
          (map cstr/trim)
          (map cstr/lower-case)
          (map parse-expr)
          vec
          add-default)]))

(defn make-expressions [feeds]
  (->> feeds
       (filter #(not= (first %) \#))
       (map parse-feed-expression)))

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

(defn get-unread-items [user]
  (let [{feeds :feeds
         positions :positions} user]
    (->> (make-expressions feeds)
         (map (fn [[feed exprs]]
                (let [pos (get positions feed (max 0 (- (get-item-count feed) 10)))]
                  (->> (get-numbered-items feed pos)
                       (filter #(item-matches % exprs))
                       (map #(assoc % :feed feed
                                      :iid [feed (:num %)]))))))
         (apply concat))))

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

(defn get-selected-items
  "Lazy sequence of items user marked for later reading."
  [user-id]
  (let [user (get-user-attrs user-id)
        feed-urls (map first (make-expressions (:feeds user)))
        items (:selected user [])]
    (sort-by #(vector (.indexOf feed-urls (:feed %))
                      (:num %))
             (map #(assoc % :iid (get-internal-id %)) items))))

(defn retrieve-item [id]
  (if (coll? id)
    (let [[feed pos] id]
      (-> (get-numbered-items feed pos)
          first
          (assoc :feed feed)))
    (let [html (content/retrieve-and-parse id)]
      {:link id
       :title (content/get-title html)
       :summary (content/summarize html)})))

(defn cache-item-content [item]
  (when-not (:content item)
    (let [feed (:feed item)
          content-ident (when feed
                          (:content-ident (get-feed-attrs feed)))]
      (future (content/cache-content! (:link item)
                                      content-ident)))))

(defn selected-add! [user-id ids]
  (let [items (map retrieve-item ids)]
    (doall (map cache-item-content items))
    (update-user-attrs! user-id update :selected into items)))

(defn selected-remove! [user-id ids]
  (letfn [(in-ids? [item]
            (let [id (get-internal-id item)]
              (some #(= id %) ids)))]
    (update-user-attrs! user-id update :selected #(remove in-ids? %))))

; === sync ===

(defn active-feeds
  "List of feeds subscribed to by at least one user."
  []
  (->> (all-users)
       (map get-user-attrs)
       (map :feeds)
       (reduce into)
       make-expressions
       (map first)
       set))

(defn sync! []
  (let [active (active-feeds)]
    (->> (keys @feed-index)
         (filter active)
         (filter #(jt/before? (next-update-time %)
                              (jt/instant)))
         (map #(future (vector % (sync-and-log-safe! %))))
         (map deref))))

(defn init-auto-sync []
  (future
    (while 42 (do (log/info "Starting sync by the timer")
                  (try
                    (doall (sync!))
                    (log/info "Sync is complete")
                    (catch Exception ex
                      (log/error ex "Sync failed")))
                  (java.lang.Thread/sleep (* 30 60 1000))))))

(defn init! []
  (send feed-index init-feed-index! (conf/param :data-dir))
  (await feed-index)
  (def auto-sync (init-auto-sync)))
