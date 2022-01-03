(ns feedcircuit-revisited.import
  (:require [datomic.client.api :as d]
            [clojure.set :refer (rename-keys)]
            [feedcircuit-revisited.schema :as schema]
            [feedcircuit-revisited.fs-backend :as fs-back]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.datomic-backend :as d-back]))

(defn import-feeds []
  (let [feeds (map fs-back/get-feed-attrs
                   (keys @fs-back/feed-index))]
    (map #(d-back/add-feed! (:feed/url %) %) feeds)))

(defn fix-link [item]
  (update item
          :item/link
          (fn [x] (if (coll? x)
                    (first x)
                    x))))

(defn fix-summary [item]
  (feed/fix-summary-and-content item nil))

(defn import-items [url]
  (let [items (->> (fs-back/get-items url 0)
                   (map (comp fix-link fix-summary))
                   (map #(rename-keys % {:item/source-id :item/id})))]
    (doseq [chunk (partition-all 1024 items)]
      (d-back/append-items! url chunk))
    (reset! d-back/cur-item-num (d-back/find-max-num))))

(defn import-all-items []
  (doall (map import-items (keys @fs-back/feed-index))))

(defn convert-item-id [subj]
  (when-let [[feed num] (fs-back/parse-item-id subj)]
    (ffirst (d/q '[:find ?i
                   :in $ ?feed ?num
                   :where [?i :item/num ?num]
                          [?i :item/feed ?f]
                          [?f :feed/url ?feed]]
                  (d/db d-back/conn) feed num))))

(defn import-user [user-id]
  (let [{sources  :user/sources
         selected :user/selected
         styles   :user/styles} (fs-back/get-user-data user-id)]
    (let [txdata
          (into
           [[:db/add "user" :user/id user-id]]
           (concat 
            (for [[site style] styles]
              [:db/add "user" :user/styles (str site " " style)])
            (for [{id :item/id} selected
                  :let [iid (convert-item-id id)]
                  :when iid]
              [:db/add "user" :user/selected iid])
            (for [src sources
                  :let [{feed :source/feed
                         filters :source/filters} src]]
              (-> (select-keys src [:source/num :source/active :source/position])
                  (assoc :source/user "user"
                         :source/feed (d-back/get-feed-attr feed :db/id))
                  (merge (if filters {:source/filters filters}))))))]
      (d/transact d-back/conn {:tx-data txdata}))))
