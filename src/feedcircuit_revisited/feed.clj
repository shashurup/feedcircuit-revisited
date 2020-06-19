(ns feedcircuit-revisited.feed
  (:require [clojure.xml :as xml]
            [clojure.set :as cset]
            [clojure.string :as cstr]
            [java-time :as jt]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.rfc822 :as rfc822]
            [clojure.core.memoize :as memz]
            [clojure.string :as s]
            [clojure.tools.logging :as log]))

(defn parse-int [s] (if s (Integer. s)))

(def block-size "Number of items in each file" 100)

; === item storage ===

(defn write-file [filename data]
  (let [tempfilename (str filename ".temp")]
    (with-open [w (io/writer tempfilename)]
      (binding [*out* w] (pr data)))
    (fs/rename tempfilename filename)))

(defn read-file [filename]
  (if (fs/exists? filename)
    (with-open [r (java.io.PushbackReader. (io/reader filename))]
      (read r))))

(def get-data (memz/memo read-file))

(defn set-data [filename data]
  (write-file filename data)
  (memz/memo-clear! get-data [filename])
  data)

(defn get-block [dir block-num]
  (get-data (str dir "/" block-num)))

(defn set-block [dir block-num data]
  (set-data (str dir "/" block-num) data))

(defn set-attrs [dir attrs]
  (set-data (str dir "/attrs") attrs))

(defn get-attrs [dir]
  (get-data (str dir "/attrs")))

(defonce dir-cache (atom {}))

(defn load-dir [dir]
  (let [block-list (->> (fs/list-dir dir)
                        (map fs/base-name)
                        (filter #(re-matches #"[0-9]+" %))
                        (map parse-int)
                        sort
                        vec)]
    {:item-count (if (empty? block-list)
                   0
                   (+ (* (dec (count block-list)) block-size)
                      (count (get-block dir (last block-list)))))
     :known-ids (->> block-list
                     (map #(read-file (str dir "/" %))) ; avoid caching all blocks
                     (apply concat)
                     (map :id)
                     (set))}))

(defn get-dir-cache [dir key]
  (let [entry (or (get @dir-cache dir)
                  (let [entry (load-dir dir)]
                    (swap! dir-cache assoc dir entry)
                    entry))]
    (get entry key)))

(defn get-known-ids [dir]
  (get-dir-cache dir :known-ids))

(defn get-item-count [dir]
  (or (get-dir-cache dir :item-count) 0))

(defn get-last-block-num [dir]
  (quot (get-item-count dir) block-size))

(defn get-items 
  "Returns lazy sequence of items in the directory dir
   beginning from the start"
  [dir start]
  (let [last-block-num (get-last-block-num dir)
        start-block (quot start block-size)
        start-offset (rem start block-size)
        items (apply concat (map #(get-block dir %)
                                 (range start-block (inc last-block-num))))]
    (nthrest items start-offset)))

(defn get-numbered-items 
  "Returns lazy numbered sequence of items
   in the directory dir beginning from the start"
  [dir start]
  (map-indexed #(assoc %2 :num (+ start %1))
               (get-items dir start)))

(defn get-internal-id [item]
  (if-let [num (:num item)]
    [(:feed item) num]
    (:link item)))

(defn append-items!
  "Appends items to the end of the list in the directory dir"
  [dir items]
  (let [last-block-num (get-last-block-num dir)
        last-block (get-block dir last-block-num)
        new-blocks (->> (concat last-block items)
                        (partition-all block-size)
                        (map vec))
        known-ids (get-known-ids dir)
        start (get-item-count dir)]
    (doseq [[num block] (map-indexed vector new-blocks)]
      (set-block dir (+ last-block-num num) block))
    (swap! dir-cache assoc dir {:item-count (+ start (count items))
                                :known-ids (cset/union known-ids
                                                       (set (map #(:id %) items)))})
    (range start (+ start (count items)))))

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

(def attr-convert {:pubDate from-rfc1123-datetime
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

(defn fetch-new-items
  "Fetches new items from the feed located at the url.
   The known-ids is a set of already fetched items."
  [url known-ids]
  (let [reply (http/get url {:as :stream})
        feed-xml (xml/parse (:body reply))
        attrs (parse-feed-details feed-xml)
        items (map parse-rss-item (extract-rss-items feed-xml))
        new-items (->> items
                       (filter #(not (contains? known-ids (:id %))))
                       (sort-by :published))]
     [attrs new-items]))

(defn average [coll]
  (quot (apply + coll) (count coll)))

; === feed handling ===

(defonce feed-dir (atom {}))

(defn load-feed-dirs []
  (->> (fs/list-dir (str (conf/param :data-dir) "/feeds"))
       (map fs/normalized)
       (filter fs/directory?)
       (map #(vector (:url (get-attrs %)) (str %)))
       (into {})))

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

(defn preproces [items attrs]
  (->> items
       (map #(fix-refs % (:url attrs)))
       (map fix-summary-and-content)))

(defonce pending-feeds (atom {}))

(defn do-add-feed! [url]
  (let [dir (dir-path url)
        [attrs new-items] (fetch-new-items url #{})]
    (fs/mkdirs dir)
    (set-attrs dir (assoc attrs :url url))
    (append-items! dir (preproces new-items
                                  (get-attrs dir)))
    (swap! feed-dir assoc url dir)
    dir))

(defn add-feed! [url]
  (letfn [(push [pending url]
            (if (pending url)
              pending
              (assoc pending url (delay (do-add-feed! url)))))]
    ;; signal that feed is being added
    (swap! pending-feeds push url))
  ;; wait until feed addition is complete
  (deref (@pending-feeds url)))

(defn sync-feed! [url]
  (let [dir (get @feed-dir url)
        known-ids (get-known-ids dir)
        [new-attrs new-items] (fetch-new-items url known-ids)]
    (set-attrs dir (merge (get-attrs dir)
                          new-attrs))
    (append-items! dir (preproces new-items
                                  (get-attrs dir)))))

(defn sync-and-log-safe! [url]
  (log/info "Getting news from" url)
  (try
    (let [result (sync-feed! url)]
      (log/info "Got" (count result) "item from" url)
      result)
    (catch Exception ex
      (log/error ex "Failed to get news from" url))))

(defn next-update-time
  "Deduce next update time for the feed located at url.
   The algorithm takes into account how often the feed is updated."
  [url]
  (let [dir (get @feed-dir url)
        last-items (get-items dir (max 0 (- (get-item-count dir) 10)))
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
  (get-attrs (@feed-dir feed)))

(defn get-feed-item-count [feed]
  (get-item-count (@feed-dir feed)))

(defn get-feed-items [feed start]
  (->> (get-numbered-items (@feed-dir feed) start)
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
                (let [dir (@feed-dir feed)
                      pos (get positions feed (max 0 (- (get-item-count dir) 10)))]
                  (->> (get-numbered-items dir pos)
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

(defn get-user-attrs [id]
  (-> (get-attrs (user-dir id))
      (assoc :id id)
      (update :unread #(apply sorted-set %))))

(defn update-user-attrs! [attrs]
  (let [dir (user-dir (:id attrs))]
    (fs/mkdirs dir)
    (set-attrs dir
               (-> attrs
                   (dissoc :id)
                   (update :unread vec)))))

(defn archive-items! [user ids]
  (let [dir (user-dir user)
        items (map (fn [[url pos]]
                     (first (get-items (get @feed-dir url) pos))) ids)]
    (fs/mkdirs dir)
    (append-items! dir items)))

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
      (-> (get-numbered-items (@feed-dir feed) pos)
          first
          (assoc :feed feed)))
    (let [html (content/retrieve-and-parse id)]
      {:link id
       :title (content/get-title html)
       :summary (content/summarize html)})))

(defn selected-add! [user-id ids]
  (let [user (get-user-attrs user-id)]
    (->> ids
       (map retrieve-item)
       (update user :selected into)
       update-user-attrs!)))

(defn selected-remove! [user-id ids]
  (let [user (get-user-attrs user-id)
        in-ids? (fn [item]
                  (let [id (get-internal-id item)]
                    (some #(= id %) ids)))]
    (update-user-attrs!
     (update user :selected #(remove in-ids? %)))))

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
    (->> (keys @feed-dir)
         (filter active)
         (filter #(jt/before? (next-update-time %)
                              (jt/instant)))
         (map #(vector % (count (sync-and-log-safe! %)))))))

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
  (reset! feed-dir (load-feed-dirs))
  (def auto-sync (init-auto-sync)))
