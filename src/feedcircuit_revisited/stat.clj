(ns feedcircuit-revisited.stat
  (:require [clojure.tools.logging :as log]
            [clojure.zip :as zip]
            [feedcircuit-revisited.backend :as backend]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.jsoup :as jsoup]))

(defn unique? [ident html]
  (let [type (first ident)]
    (condp = type
      :id true
      :class (= (count (content/find-by-identifier ident html)) 1)
      false)))

(defn find-content [item]
  [item
   (->> (:item/link item)
        content/retrieve-and-parse
        content/find-content-element)])

(defn get-content-identifier [[_ zipper]]
  (let [ident (content/get-identifier (zip/node zipper))]
    (when (unique? ident (zip/root zipper))
      ident)) )

(defn compute-content-identifier [data]
  (->> data
       (map get-content-identifier)
       frequencies
       (apply max-key second)
       first))

(defn calc-ratio [[item zipper]]
  (float (/ (content/text-size-recursively (zip/node zipper))
            (content/calculate-size (:item/summary item)))))

(defn average [xs]
  (when (not-empty xs)
    (/ (apply + xs) (count xs))))

(defn compute-content-to-summary-ratio [data]
  (->> data
       (filter (comp :item/summary first))
       (map calc-ratio)
       average))

(defn compute [url]
  (let [[_ items] (feed/fetch-items url)
        data (->> items
                  (remove :item/content)
                  (map #(future (find-content %)))
                  (map deref)
                  (filter second))]
    (when (not-empty data)
      {:feed/content-ident (compute-content-identifier data)
       :feed/content-to-summary-ratio (compute-content-to-summary-ratio data)})))

(defn collect-feed! [feed]
  (let [stat (compute feed)]
    (backend/update-feed! feed stat)
    stat))

(defn collect-and-log-safe! [feed]
  (try
    (log/info "Collecting statistics for" feed)
    (let [stat (collect-feed! feed)]
      (log/info "Statistics for" feed "are" stat)
      stat)
    (catch Exception ex
      (log/error ex "Failed to collect statistics for" feed))))

(defn collect! []
  (map collect-and-log-safe! (backend/active-feed-urls)))

(defn init-collector []
  (future
    (while 42 (do (log/info "Starting statistics collection")
                  (doall (collect!))
                  (log/info "Statistics collection is complete")
                  (java.lang.Thread/sleep (* 12 60 60 1000))))))

(defn init! []
  (def collector (init-collector)))
