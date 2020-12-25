(ns feedcircuit-revisited.stat
  (:require [clojure.tools.logging :as log]
            [clojure.zip :as zip]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.storage :as storage]
            [feedcircuit-revisited.jsoup :as jsoup]))

(defn unique? [ident html]
  (let [type (first ident)]
    (condp = type
      :id true
      :class (= (count (content/find-by-identifier ident html)) 1)
      false)))

(defn get-content-identifier [url]
  (when-let [zipper (->> url
                         content/retrieve-and-parse
                         content/find-content-element)]
    (let [ident (content/get-identifier (zip/node zipper))]
      (if (unique? ident (zip/root zipper))
        ident))))

(defn value [x] x)

(defn compute [url]
  (let [[_ items] (feed/fetch-items url)]
    (->> items
         (map :link)
         (map #(future (get-content-identifier %)))
         (map deref)
         (group-by value)
         (map (fn [[k v]] [k (count v)]))
         (apply max-key second)
         (first))))

(defn write-content-ident [index url ident]
  (let [dir (get-in index [url :dir])]
    (storage/set-attrs dir
                       (assoc (storage/get-attrs dir)
                              :content-ident ident)))
  index)

(defn collect-feed! [url]
  (let [ident (compute url)]
    (send feed/feed-index write-content-ident url ident)
    ident))

(defn collect-and-log-safe! [url]
  (try
    (log/info "Collecting statistics for" url)
    (let [ident (collect-feed! url)]
      (log/info "Content identifier for" url "is" ident)
      ident)
    (catch Exception ex
      (log/error ex "Failed to collect statistics for" url))))

(defn collect! []
  (map collect-and-log-safe! (feed/active-feeds)))

(defn init-collector []
  (future
    (while 42 (do (log/info "Starting statistics collection")
                  (doall (collect!))
                  (log/info "Statistics collection is complete")
                  (java.lang.Thread/sleep (* 12 60 60 1000))))))

(defn init! []
  (def collector (init-collector)))
