(ns feedcircuit-revisited.feed
  (:require [clojure.xml :as xml]
            [clojure.set :as cset]
            [clojure.string :as cstr]
            [java-time :as jt]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [feedcircuit-revisited.content :as content]
            [clojure.core.memoize :as memz]
            [clojure.string :as s]
            [clojure.tools.logging :as log]))

(defn parse-int [s] (if s (Integer. s)))

(def config {:root-dir "./fc-data"})
(def block-size 100)

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
  (memz/memo-clear! get-data [filename]))

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

(defn get-items [dir start]
  (let [last-block-num (get-last-block-num dir)
        start-block (quot start block-size)
        start-offset (rem start block-size)
        items (apply concat (map #(get-block dir %)
                                 (range start-block (inc last-block-num))))]
    (nthrest items start-offset)))

(defn get-numbered-items [dir start]
  (map-indexed #(assoc %2 :num (+ start %1))
               (get-items dir start)))

(defn append-items! [dir items]
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

(defn from-rfc1123-datetime [attr] (str (jt/instant (jt/formatter :rfc-1123-date-time)
                                                    (content-str attr))))

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

(defn parse-rss-item [item]
  (reduce parse-rss-item-attribute {} (:content item)))

(defn find-root [feed-xml]
  (or (first (filter #(= (:tag %) :channel)
                     (:content feed-xml)))
             feed-xml))

(defn extract-rss-items [feed-xml]
  (let [root (find-root feed-xml)]
    (filter #(contains? #{:item :entry} (:tag %)) (:content root))))

(defn parse-feed-details [feed-xml]
  (->> (:content (find-root feed-xml))
       (filter #(not (contains? #{:item :entry} (:tag %))))
       (reduce parse-rss-item-attribute {})))

(defn fetch-new-items [url known-ids]
  (let [feed-xml (xml/parse url)
        attrs (parse-feed-details feed-xml)
        items (map parse-rss-item (extract-rss-items feed-xml))
        new-items (->> items
                       (filter #(not (contains? known-ids (:id %))))
                       (sort-by :published))]
     [attrs new-items]))

(defn average [coll]
  (quot (apply + coll) (count coll)))

(defn long-content? [items]
  (->> items
       (map :summary)
       (map count)
       average
       (<= 1024)))

; === feed handling ===

(defn load-feed-dirs []
  (->> (fs/list-dir (str (:root-dir config) "/feeds"))
       (map fs/normalized)
       (filter fs/directory?)
       (map #(vector (:url (get-attrs %)) (str %)))
       (into {})))

(defonce feed-dir (atom (load-feed-dirs)))

(defn dir-name [url]
  (-> url
      (cstr/replace "http://" "")
      (cstr/replace "https://" "")
      (cstr/replace "/" ".")))

(defn dir-path [url]
  (str (fs/normalized (str (:root-dir config) "/feeds/" (dir-name url)))))

(defn extract-summary [item]
  (let [content (:summary item)
        summary (content/summarize content)]
    (if summary
      (assoc item :content content
             :summary summary)
      item)))

(defn preproces [items attrs]
  (if (:long-content attrs)
    (map extract-summary items)
    items))

(defn add-feed! [url]
  (let [dir (dir-path url)
        [attrs new-items] (fetch-new-items url #{})]
    (fs/mkdirs dir)
    (swap! feed-dir assoc url dir)
    (set-attrs dir (assoc attrs
                          :url url
                          :long-content (long-content? new-items)))
    (append-items! dir (preproces new-items
                                  (get-attrs dir)))))

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

(defn next-update-time [url]
  (let [dir (get @feed-dir url)
        last-items (get-items dir (max 0 (- (get-item-count dir) 10)))
        dates (->> last-items
                   (map :published)
                   (map #(jt/instant (jt/formatter :iso-date-time) %)))
        deltas (->> dates
                    (map jt/to-millis-from-epoch)
                    (partition 2 1)
                    (map (fn [[b e]] (- e b))))]
    (if (empty? deltas)
      (jt/instant)
      (jt/plus (apply jt/max dates)
               (jt/min (jt/hours 24)
                       (jt/millis (quot (apply + deltas)
                                        (count deltas))))))))

(defn sync! []
  (->> (keys @feed-dir)
       (filter #(jt/before? (next-update-time %)
                            (jt/instant)))
       (map #(vector % (count (sync-and-log-safe! %))))))

(defonce timer (future
                 (while 42 (do (log/info "Starting sync by the timer")
                               (doall (sync!))
                               (java.lang.Thread/sleep (* 30 60 1000))))))

; === user handling ===

(defn parse-feed-expression [expr]
  (let [parse-expr #(let [include (not= \! (first %))]
                      [(if include % (apply str (rest %)))
                       include])
        [url filters] (cstr/split expr #" " 2)]
    [url
     (if (not (cstr/blank? filters))
       (->> (cstr/split filters #",")
            (map cstr/trim)
            (map cstr/lower-case)
            (map parse-expr)
            vec))]))

(defn get-attrs-for-filter [item]
  (->> (concat (:author item) (:category item))
       (map cstr/lower-case)
       vec))

(defn item-matches [item expressions]
  (if (empty? expressions)
    true
    (let [attrs (get-attrs-for-filter item)]
      (if (empty? attrs)
        true
        (first (for [attr attrs
                     [term verdict] expressions
                     :when (= attr term)]
                 verdict))))))

(defn get-user-items [user count]
  (let [{feeds :feeds
         positions :positions} user]
    (->> feeds
         (map parse-feed-expression)
         (map (fn [[feed exprs]]
                (->> (get-numbered-items (get @feed-dir feed)
                                         (get positions feed 0))
                     (filter #(item-matches % exprs))
                     (map #(assoc % :feed feed)))))
         (apply concat)
         (take count))))

(defn user-dir [id]
  (str (:root-dir config) "/users/" id))

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

(defn select-items! [user ids]
  (let [dir (user-dir user)
        items (map (fn [[url pos]]
                     (first (get-items (get @feed-dir url) pos))) ids)]
    (fs/mkdirs dir)
    (append-items! dir items)))

(defn get-selected-items [user-id]
  (let [dir (user-dir user-id)
        user (get-user-attrs user-id)]
    (map #(first (get-numbered-items dir %))
         (:unread user))))
