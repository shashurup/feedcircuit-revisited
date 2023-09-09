(ns feedcircuit-revisited.migration
  (:require [datomic.client.api :as d]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
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

(defn convert-selected-history []
  (let [txdata (for [[u s] (d/q '[:find ?u ?s
                                  :where [?u :user/selected ?s _ false]]
                                (d/history (d/db d-back/conn)))]
                 {:archive/user u :archive/selected s})]
    (d/transact d-back/conn {:tx-data txdata})))

(defn dump-entity
  ([attr] (dump-entity attr '[*] identity))
  ([attr pattern] (dump-entity attr pattern identity))
  ([attr pattern f]
   (map (comp f
              #(dissoc % :db/id :item/feed+id :item/feed+num)
              first)
        (d/qseq '[:find (pull ?e pattern)
                  :in $ ?attr pattern
                  :where (?e ?attr _)]
                (d/db d-back/conn)
                attr
                pattern))))

(defn make-item-ref [{eid :db/id}]
  (let [{link :item/link
         {url :feed/url} :item/feed
         num :item/num} (d/pull (d/db d-back/conn)
                                '[:item/link {:item/feed [:feed/url]} :item/num]
                                eid)]
    (if url [url num] link)))

(defn dump []
  (concat 
   (dump-entity :feed/url)
   (dump-entity :item/link
                '[* {:item/feed [:feed/url]}]
                #(if (:item/feed %)
                   (update % :item/feed (comp vec first))
                   %))
   (dump-entity :user/id
                '[*]
                #(update % :user/selected (fn [old] (vec (map make-item-ref old)))))
   (dump-entity :source/user
                '[* {:source/user [:user/id]} {:source/feed [:feed/url]}]
                (comp
                 #(update % :source/user (comp vec first))
                 #(update % :source/feed (comp vec first))))
   (dump-entity :archive/user
                '[* {:archive/user [:user/id]}]
                (comp
                 #(update % :archive/user (comp vec first))
                 #(update % :archive/selected make-item-ref)))))

(defn save-dump [dump fname]
  (with-open [w (io/writer fname)]
    (binding [*out* w]
      (doall (map prn dump)))))

(defn read-dump [fname f]
  (with-open [r (io/reader fname)]
    (let [pbr (java.io.PushbackReader. r)]
      (f (take-while not-empty (repeatedly #(edn/read {:eof nil} pbr)))))))

(defn entity [subj]
  (namespace (first (keys subj))))

(defn lookup-item [subj]
  (if (string? subj)
    (ffirst (d/q '[:find ?i
                   :in $ ?link
                   :where [?i :item/link ?link]]
                 (d/db d-back/conn)
                 subj))
    (let [[url num] subj]
      (ffirst (d/q '[:find ?i
                     :in $ ?url ?num
                     :where [?i :item/num ?num]
                     [?f :feed/url ?url]
                     [?i :item/feed ?f]]
                   (d/db d-back/conn)
                   url
                   num)))))

(defn resolve-item-id [item]
  (cond
    (:archive/selected item) (update item :archive/selected lookup-item)
    (:user/selected item) (update item
                                  :user/selected
                                  #(vec (map lookup-item %)))
    :else item))

(defn push-to-db [data]
  (doseq [batch (partition-all 1000 data)]
    (doseq [txdata (partition-by entity batch)]
      (let [txdata (map resolve-item-id txdata)]
        (prn (first txdata))
        (d/transact d-back/conn {:tx-data txdata})))))
