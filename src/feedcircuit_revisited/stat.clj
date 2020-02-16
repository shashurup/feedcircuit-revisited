(ns feedcircuit-revisited.stat
  (:require [clojure.tools.logging :as log]
            [clojure.zip :as zip]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.feed :as feed]
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
  (let [[_ items] (feed/fetch-new-items url #{})]
    (->> items
         (map :link)
         (map get-content-identifier)
         (group-by value)
         (map (fn [[k v]] [k (count v)]))
         (apply max-key second)
         (first))))

(defn collect-feed! [url]
  (let [feed-dir (@feed/feed-dir url)
        attrs (feed/get-attrs feed-dir)]
    (feed/set-attrs feed-dir
                    (assoc attrs :content-ident (compute url)))))

(defn collect! []
  (map collect-feed! (feed/active-feeds)))

(defn init-collector []
  (future
    (while 42 (do (log/info "Starting statistics collection")
                  (try
                    (doall (collect!))
                    (log/info "Statistics collection is complete")
                    (catch Exception ex
                      (log/error ex "Statistics collection failed")))
                  (java.lang.Thread/sleep (* 12 60 60 1000))))))

(defn init! []
  (def collector (init-collector)))
