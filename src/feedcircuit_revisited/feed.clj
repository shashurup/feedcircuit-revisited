(ns feedcircuit-revisited.feed
  (:require [clojure.xml :as xml]
            [clojure.set :as cset]
            [clojure.string :as cstr]
            [clojure.edn :as edn]
            [java-time :as jt]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [feedcircuit-revisited.backend :as backend]
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
  (let [known-ids (backend/get-known-ids feed (map :id items))]
    (->> items
         (remove #(known-ids (:id %)))
         (map #(fix-refs % feed))
         (map #(fix-summary-and-content % self-containing))
         (sort-by :published))))

(defn add-feed! [url]
  (let [[attrs items] (fetch-items url)]
    (backend/add-feed! url attrs)
    (backend/append-items! url (prepare-items url nil items))))

(defn sync-feed! [url]
  (let [attrs (backend/get-feed-attrs url)
        self-containing (self-containing-feed? attrs)
        [new-attrs items] (fetch-items url)]
    (backend/update-feed! url new-attrs)
    (backend/append-items! url (prepare-items url self-containing items))))

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
  (let [last-items (take 16 (backend/get-items-backwards url))
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
; === sync ===

(defn active-feeds
  "List of feeds subscribed to by at least one user."
  []
  (->> (backend/all-users)
       (map backend/get-user-data)
       (map #(map :feed (filter :active (:sources %))))
       (reduce into)
       set))

(defn sync! []
  (let [active (active-feeds)]
    (->> (keys (backend/all-feeds))
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
  (def auto-sync (init-auto-sync)))
