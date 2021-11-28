(ns feedcircuit-revisited.datomic-backend
  (:require [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.schema :as schema]
            [feedcircuit-revisited.utils :as u]
            [datomic.client.api :as d]
            [clojure.set :refer (rename-keys)]
            [clojure.string :refer (split)]
            [me.raynes.fs :as fs]))

(def content-dir)

(def client)
(def conn)

(defn find-max-num []
  (->> (d/index-pull (d/db conn)
                     {:index :avet
                      :selector [:item/num]
                      :start [:item/num]
                      :reverse true})
       ffirst
       second))

(defonce cur-item-num (atom 0))

; todo initialize cur-item-num

(def feed-attrs (conj (map :db/ident schema/feed-schema) :db/id))

(defn add-feed! [url attrs]
  (let [attrs (select-keys (assoc attrs :feed/url url)
                           feed-attrs)]
    (d/transact conn {:tx-data [attrs]})))

(def update-feed! add-feed!)

(defn get-feed-attr-by-int-id [id attr]
  (let [id? (= attr :feed/id)
        attr (if id? :db/id attr)
        conv-fn (if id? str identity)]
    (-> (d/pull (d/db conn) [attr] id)
        (get attr)
        conv-fn)))

(defn get-feed-attr-by-id [id attr]
  (get-feed-attr-by-int-id (u/as-int id) attr))

(defn get-feed-attr [url attr]
  (get-feed-attr-by-int-id [:feed/url url] attr))


(def item-attrs (conj (map :db/ident schema/item-schema) :db/id))

(defn remove-nils [subj]
  (let [nil-keys (remove #(get subj %) (keys subj))]
    (apply dissoc subj nil-keys)))

(defn augment [item feed]
  (let [{content :item/content
         source-id :item/id} item]
    (assoc item
           :db/id source-id
           :item/feed feed
           :item/feed+id (str feed "+" source-id)
           :item/has-content (boolean (not-empty content))
           :item/num (or (:item/num item)
                         (swap! cur-item-num inc)))))

(defn clean [item]
  (remove-nils (select-keys item item-attrs)))

(defn append-items! [url items]
  (let [feed (get-feed-attr url :db/id)
        items (map #(augment % feed)
                   (u/distinct-by :item/id items))
        txdata (vec (map clean items))]
    (let [tempids (:tempids (d/transact conn {:tx-data txdata}))]
      (doseq [{tempid :item/id
               content :item/content} (filter :item/has-content items)
              :let [fname (get tempids tempid)]]
        (u/write-file (str content-dir "/" fname) content)))
    (count items)))

(defn add-content! [uid content]
  (d/transact conn {:tx-data [{:db/id (u/as-int uid)
                               :item/has-content true}]})
  (u/write-file (str content-dir "/" uid) content))

(defn adapt-item [item]
  (let [id (str (:db/id item))
        {feed-id :db/id
         feed-title :feed/title} (:item/feed item)]
    (-> item
        (assoc :item/id id)
        (merge (when feed-title {:feed/title feed-title}))
        (merge (when feed-id {:item/feed (str feed-id)}))
        (dissoc :db/id))))

(def item-list-attrs [:db/id
                      :item/link
                      :item/title
                      :item/summary
                      :item/num])

(defn get-items
  ([feed]
   (get-items feed nil nil))
  ([feed start]
   (get-items feed start nil))
  ([feed start reverse]
   (let [feed-id (u/as-int feed)
         start (or start (when reverse Long/MAX_VALUE))]
     (->> (d/index-pull (d/db conn)
                        {:index :avet
                         :selector (conj item-list-attrs :item/feed)
                         :start [:item/feed+num [feed-id start]]
                         :reverse reverse})
          (take-while #(= (get-in % [:item/feed :db/id]) feed-id))
          (map adapt-item)))))

(defn get-items-backwards
  ([feed] (get-items feed nil true))
  ([feed start] (get-items feed start true)))

(defn get-item [uid]
  (let [item (d/pull (d/db conn) '[*] (u/as-int uid))
        content (u/read-file (str content-dir "/" (:db/id item)))]
    (merge (adapt-item item)
           (when content {:item/content content}))))

(defn known-ids [_ ids]
  (->> (d/q '[:find (pull ?i [:item/source-id])
              :in $ [?src-id ...]
              :where [?i :item/source-id ?src-id]]
            (d/db conn) ids)
       (map first)
       (map :item/source-id)
       set))

(defn item-id? [subj] (re-matches #"\d+" subj))

(defn active-feed-urls []
  (ffirst 
   (d/q '[:find (distinct ?url)
          :where [_ :source/feed ?f]
          [?f :feed/url ?url]]
        (d/db conn))))

(defn parse-style [subj] (split subj #" " 2))

(defn adapt-source [subj]
  (let [feed (-> (:source/feed subj)
                 (update :db/id str)
                 (rename-keys {:db/id :source/feed}))]
    (-> subj
        (dissoc :source/feed)
        (rename-keys {:db/id :source/id})
        (update :source/id str)
        (merge feed))))

(defn get-user-data [user-id & opts]
  (let [selected-attrs (if (some #{:selected/details} opts)
                         (conj item-list-attrs
                               {:item/feed [:db/id :feed/title]})
                         [:db/id])
        feed-attrs (cond
                     (some #{:sources/feed-details} opts) '[*]
                     (some #{:sources/feed-title} opts) [:db/id :feed/url :feed/title]
                     :else [:db/id :feed/url])
        source-attrs [:db/id
                      {:source/feed feed-attrs}
                      :source/num
                      :source/active
                      :source/filters
                      :source/position]]
    (-> (d/pull (d/db conn)
                [:user/id
                 :user/styles
                 {:user/selected selected-attrs}
                 {[:source/_user :as :user/sources] source-attrs}]
                [:user/id user-id])
        (update :user/selected #(map adapt-item %))
        (update :user/styles #(map parse-style %))
        (update :user/sources #(map adapt-source %)))))

(defn update-settings! [user-id sources styles]
  )

(defn update-positions! [user-id positions]
  (let [txdata (vec (for [[src pos] positions]
                      [:db/add (u/as-int src) :source/position pos]))]
    (d/transact conn {:tx-data txdata})))

(defn selected-change! [user-id ids op]
  (let [txdata (vec (for [id ids]
                      [op [:user/id user-id] :user/selected (u/as-int id)]))]
    (when-not (empty? txdata)
      (d/transact conn {:tx-data txdata}))))

(defn selected-add! [user-id ids]
  ; todo handle item by value
  (selected-change! user-id ids :db/add))

(defn selected-remove! [user-id ids]
  (selected-change! user-id ids :db/retract))

(defn init-impl! []
  (def content-dir (conf/param :datomic :content-dir))
  (when (not-empty content-dir)
    (fs/mkdirs content-dir))
  (def client (d/client (conf/param :datomic :client)))
  (let [db-name (conf/param :datomic :db-name)]
    (when-not ((set (d/list-databases client {})) db-name)
      (d/create-database client {:db-name db-name})
      (let [conn (d/connect client {:db-name db-name})]
        (d/transact conn {:tx-data schema/schema})))
    (def conn (d/connect client {:db-name db-name})))
  (reset! cur-item-num (or (find-max-num) 0)))
