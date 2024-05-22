(ns feedcircuit-revisited.migration
  (:require [datomic.client.api :as d]
            [datalevin.core :as dtlv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer (rename-keys)]
            [feedcircuit-revisited.schema :as schema]
            [feedcircuit-revisited.fs-backend :as fs-back]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.datomic-backend :as d-back]
            [feedcircuit-revisited.ui :as ui]))

(defprotocol Datalog
  (q [this query params])
  (qseq [this query params])
  (pull [this pattern id])
  (transact! [this tx-data]))

(deftype Datomic [conn]
  Datalog
  (q [this query params]
    (apply d/q query (d/db conn) params))
  (qseq [this query params]
    (apply d/qseq query (d/db conn) params))
  (pull [this pattern id]
    (d/pull (d/db conn) pattern id))
  (transact! [this tx-data]
    (d/transact conn {:tx-data tx-data})))

(deftype Datalevin [conn]
  Datalog
  (q [this query params]
    (apply dtlv/q query (dtlv/db conn) params))
  (qseq [this query params]
    (apply dtlv/q query (dtlv/db conn) params))
  (pull [this pattern id]
    (dtlv/pull (dtlv/db conn) pattern id))
  (transact! [this tx-data]
    (dtlv/transact! conn tx-data)))

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
  ([c attr] (dump-entity c attr '[*] identity))
  ([c attr pattern] (dump-entity c attr pattern identity))
  ([c attr pattern f]
   (map (comp f
              #(dissoc % :db/id :item/feed+id :item/feed+num)
              first)
        (qseq c
              '[:find (pull ?e pattern)
                :in $ ?attr pattern
                :where (?e ?attr _)]
              [attr pattern]))))

(defn make-item-ref [c {eid :db/id}]
  (let [{link :item/link
         {url :feed/url} :item/feed
         num :item/num} (pull c '[:item/link {:item/feed [:feed/url]} :item/num] eid)]
    (if url [url num] link)))

(defn dump [c]
  (let [make-item-ref #(make-item-ref c %)]
    (concat 
     (dump-entity c :feed/url)
     (dump-entity c
                  :item/link
                  '[* {:item/feed [:feed/url]}]
                  #(if (:item/feed %)
                     (update % :item/feed (comp vec first))
                     %))
     (dump-entity c
                  :user/id
                  '[*]
                  #(update % :user/selected (fn [old] (mapv make-item-ref old))))
     (dump-entity c
                  :source/user
                  '[* {:source/user [:user/id]} {:source/feed [:feed/url]}]
                  (comp
                   #(update % :source/user (comp vec first))
                   #(update % :source/feed (comp vec first))))
     (dump-entity c
                  :archive/user
                  '[* {:archive/user [:user/id]}]
                  (comp
                   #(update % :archive/user (comp vec first))
                   #(update % :archive/selected make-item-ref))))))

(defn save-dump [dump fname]
  (with-open [w (io/writer fname)]
    (binding [*out* w]
      (doall (map prn dump)))))

(defn dump-datomic [conn fname]
  (save-dump (dump (Datomic. conn)) fname))

(defn entity [subj]
  (namespace (first (keys subj))))

(defn lookup-item [subj c]
  (if (string? subj)
    (ffirst (q c
               '[:find ?i
                 :in $ ?link
                 :where [?i :item/link ?link]]
               [subj]))
    (let [[url num] subj]
      (ffirst (q c
                 '[:find ?i
                   :in $ ?url ?num
                   :where [?i :item/num ?num]
                   [?f :feed/url ?url]
                   [?i :item/feed ?f]]
                 [url num])))))

(defn resolve-item-id [item c]
  (let [lookup #(lookup-item % c)]
    (cond
      (:archive/selected item) (update item :archive/selected lookup)
      (:user/selected item) (update item
                                    :user/selected
                                    #(mapv lookup %))
      :else item)))

(defn push-to-db [data c]
  (doseq [batch (partition-all 1000 data)]
    (doseq [txdata (partition-by entity batch)]
      (let [txdata (map #(resolve-item-id % c) txdata)]
        (prn (first txdata))
        (transact! c txdata)))))

(defn load-dump [fname c f]
  (with-open [r (io/reader fname)]
    (let [pbr (java.io.PushbackReader. r)]
      (f (take-while not-empty (repeatedly #(edn/read {:eof nil} pbr))) c))))

(defn load-dump-to-datomic [fname conn]
  (load-dump fname (Datomic. conn) push-to-db))

(defn load-dump-to-datalevin [fname conn]
  (load-dump fname (Datalevin. conn) push-to-db))

(defn obj-type [obj]
  (cond (:feed/url obj) :feed
        (:item/feed obj) :item
        (:user/id obj) :user
        (:source/user obj) :source
        (:archive/user obj) :archive
        :else :unknown))

(defn figure-position [ctx feed src]
  (let [pos (:source/position src 0)
        last-num (get-in ctx [:feeds feed :feed/last-num] 0)]
    (if (> pos last-num)
      (fs-back/get-item-count feed)
      (get-in ctx [:ctx feed pos] 0))))

(defn add-dump-obj [ctx obj]
  (condp = (obj-type obj)
    :feed (assoc-in ctx [:feeds (:feed/url obj)] obj)
    :item (let [feed (second (:item/feed obj))
                num (:item/num obj)]
            (-> ctx
                (update-in [:ctx feed]
                           #(assoc % num (count %)))
                (update-in [:items feed]
                           #(conj (or %1 []) %2)
                           (-> obj
                               (dissoc :item/feed :item/num)))))
    :user (assoc-in ctx [:users (:user/id obj)]
                    (rename-keys obj {:user/id :id
                                      :user/styles :styles
                                      :user/selected :selected}))
    :source (let [src (update obj :source/feed second)
                  feed (:source/feed src)
                  user (second (:source/user src))]
              (-> ctx
                  (update-in [:source-lines user]
                             #(conj (or %1 []) (fs-back/source->str src)))
                  (assoc-in [:source-pos user feed]
                            (figure-position ctx feed src))))
    ctx))

(defn convert-selected-to-fs [user ctx]
  (update user :selected
          (fn [selected]
            (map (fn [subj]
                   (if (vector? subj)
                     (let [[feed num] subj]
                       (let [num (get-in ctx [feed num] 0)]
                         (fs-back/get-item (str num "," feed))))
                     {:item/link (str subj)
                      :item/id (str subj)
                      :item/title "External item (partially imported)"}))
                 selected))))

(defn store-to-fs! [{feeds :feeds
                     items :items
                     users :users
                     source-lines :source-lines
                     source-pos :source-pos
                     ctx :ctx}]
  (doseq [[url feed] feeds]
    (fs-back/add-feed! url (dissoc feed :feed/last-num)))
  (doseq [[feed _items] items]
    (fs-back/append-items! feed _items))
  (doseq [[id user] users]
    (fs-back/update-user-attrs! id merge (convert-selected-to-fs user ctx)))
  (doseq [[id lines] source-lines]
    (fs-back/update-user-attrs! id update :feeds #(into (or %1 []) lines)))
  (doseq [[id positions] source-pos]
    (fs-back/update-positions! id positions)))

(defn push-to-fs [data _]
  (reduce (fn [ctx block]
            (let [composed (reduce add-dump-obj
                                   {:ctx ctx}
                                   block)]
              (store-to-fs! composed)
              (merge-with merge ctx (:ctx composed))))
          {}
          (partition-all 4096 data))
  nil)

(defn load-dump-to-fs [fname]
  (load-dump fname nil push-to-fs))
