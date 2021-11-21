(ns feedcircuit-revisited.import
  (:require [datomic.client.api :as d]
            [feedcircuit-revisited.schema :as schema]
            [feedcircuit-revisited.fs-backend :as bfs]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.datomic-backend :as bda]))

(defn import-feeds []
  (let [feeds (map bfs/get-feed-attrs
                   (keys @bfs/feed-index))]
    (map #(bda/add-feed! (:feed/url %) %) feeds)))

(defn remove-dups [items]
  (->> items
       (map #(vector (:item/source-id %) %))
       (into {})
       vals
       (sort-by :item/num)))

(defn fix-link [item]
  (update item
          :item/link
          (fn [x] (if (coll? x)
                    (first x)
                    x))))

(defn fix-summary [item]
  (feed/fix-summary-and-content item nil))

(defn import-items [url]
  (let [items (map (comp fix-link fix-summary) (bfs/get-items url 0))]
    (bda/append-items! url (remove-dups items))
    (reset! bda/cur-item-num (bda/find-max-num))))
