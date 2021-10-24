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
               :description :summary
               :logo :image
               :dc:creator :author})

(def array-attrs #{:author :dc:creator :category :contributor})

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

(defn nested-tag-content [tag subj]
  (->> subj
       :content
       (filter #(= tag (:tag %)))
       first
       content-str))

(defn parse-author [subj]
  (if (string? (first (:content subj)))
    (content-str subj)
    (nested-tag-content :name subj)))

(def attr-convert {:pubDate parse-rss-datetime
                   :link get-link-url
                   :author parse-author
                   :image #(nested-tag-content :url %)
                   :contributor #(nested-tag-content :name %)})

(defn parse-rss-item-attribute [item attr]
  (let [tag (:tag attr)
        conv (get attr-convert tag content-str)
        upd-fn (if (contains? array-attrs tag) (fn [old new] (conj (or old []) new))
                                               (fn [old new] new))]
    (if-let [attr-val (conv attr)]
      (update item (get attr-map tag tag) upd-fn attr-val)
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

(defn get-feed-attrs [feed]
  (storage/get-attrs (get-dir feed)))

(defn set-feed-attrs [feed attrs]
  (storage/set-attrs (get-dir feed) attrs))

(defn get-unique-id [item]
  (if-let [num (:num item)]
    (str num "," (:feed item))
    (:link item)))

(defn as-int [subj]
  (try
    (Long/parseLong subj)
    (catch NumberFormatException _ nil)))

(defn parse-unique-id [uid]
  (if uid
    (if-let [[_ ord-num feed] (re-matches #"([0-9]+),(.*)" uid)]
      [feed (as-int ord-num)]
      uid)))

(defn add-uid-and-feed-title [item]
  (assoc item
         :uid (get-unique-id item)
         :feed-title (:title (get-feed-attrs (:feed item)))))

(defn get-numbered-items 
  "Returns lazy numbered sequence of items
   in the directory dir beginning from the start"
  [feed start]
  (let [dir (get-dir feed)]
    (map-indexed #(assoc %2 :num (+ start %1))
                 (storage/get-items dir start))))

(defn get-feed-items [feed start]
  (->> (get-numbered-items feed start)
       (map #(add-uid-and-feed-title (assoc % :feed feed)))))

(defn get-item [uid]
  (let [feed-pos (parse-unique-id uid)]
    (if (coll? feed-pos)
      (let [[feed pos] feed-pos]
        (first (get-feed-items feed pos))))))

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

(defn get-known-ids [feed ids]
  (get-in @feed-index [feed :known-ids]))

(defn add-feed2! [feed attrs]
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

(defn append-items2! [feed items]
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

(defn self-containing-feed? [attrs]
  (when-let [ratio (:content-to-summary-ratio attrs)]
    (< (max (- 1 ratio) (- ratio 1)) 0.2)))

(defn fix-summary-and-content
  "Summary may be absent from item, in this case
   it is deduced from content. Summary may be too long,
   in this case it is made shorter. At the same time
   content may be absent, in this case summary takes its place."
  [item self-containing]
  (let [summary (:summary item)
        content (:content item)]
    (update (if (empty? summary)
              (assoc item :summary (or content ""))
              (if (and (empty? content) self-containing)
                (assoc item :content summary)
                item))
            :summary #(if (> (count %) content/summary-soft-limit)
                        (content/summarize %)
                        %))))

(defn fix-refs
  "Make all references in content and summary absolute"
  [item base-url]
  (cond-> item
    (:summary item) (update :summary content/make-refs-absolute base-url)
    (:content item) (update :content content/make-refs-absolute base-url)))

(defn prepare-items [feed self-containing items]
  (let [known-ids (get-known-ids feed (map :id items))]
    (->> items
         (remove #(known-ids (:id %)))
         (map #(fix-refs % feed))
         (map #(fix-summary-and-content % self-containing))
         (sort-by :published))))

(defn add-feed! [url]
  (let [[attrs items] (fetch-items url)]
    (add-feed2! url attrs)
    (append-items2! url (prepare-items url nil items))))

(defn sync-feed! [url]
  (let [attrs (get-feed-attrs url)
        self-containing (self-containing-feed? attrs)
        [new-attrs items] (fetch-items url)]
    (update-feed! url new-attrs)
    (append-items2! url (prepare-items url
                                       self-containing
                                       items))))

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
                       (map #(add-uid-and-feed-title (assoc % :feed feed)))))))
         (apply concat))))

(defn get-selected-among-unread [user]
  (let [{selected :selected
         positions :positions} user]
    (->> selected
         (filter :feed)
         (remove #(< (:num %) (get positions (:feed %))))
         (map get-unique-id)
         set)))

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
             (map add-uid-and-feed-title items))))

(defn retrieve-item [uid]
  (or (get-item uid)
      (let [html (content/retrieve-and-parse uid)]
        {:link uid
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
            (let [id (get-unique-id item)]
              (some #(= id %) ids)))]
    (update-user-attrs! user-id update :selected #(remove in-ids? %))))

(defn get-selected-for-feed [user feed]
  (let [selected (:selected user)]
    (->> selected
         (filter #(= (:feed %) feed))
         (map get-unique-id)
         set)))

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
