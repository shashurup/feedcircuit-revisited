(ns feedcircuit-revisited.datomic-backend
  (:require [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.schema :as schema]
            [feedcircuit-revisited.utils :as u]
            [datomic.client.api :as d]
            [clojure.set :refer (rename-keys difference)]
            [clojure.string :refer (split join)]
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

(defn remove-nils [subj]
  (let [nil-keys (remove #(get subj %) (keys subj))]
    (apply dissoc subj nil-keys)))

(defn add-feed! [url attrs]
  (let [attrs (-> attrs
                  (assoc :feed/url url)
                  (select-keys feed-attrs)
                  (remove-nils))]
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

(defn prepare-item-content [item]
  (let [content (:item/content item)]
    (-> item
        (assoc :item/has-content (boolean (not-empty content)))
        (dissoc :item/content))))

(defn clean-item [item]
  (remove-nils (select-keys item item-attrs)))

(defn prepare-item [item feed]
  (let [source-id (:item/id item)]
    (-> item
        prepare-item-content
        (assoc :db/id source-id
               :item/feed feed
               :item/feed+id (str feed "+" source-id)
               :item/num (or (:item/num item)
                             (swap! cur-item-num inc)))
        clean-item)))

(defn append-items! [url items]
  (let [feed (get-feed-attr url :db/id)
        items (->> items
                   (u/distinct-by :item/id)
                   (map #(prepare-item % feed))
                   vec)
        txdata (when-let [ln (:item/num (last items))]
                 (conj items [:db/add feed :feed/last-num ln]))]
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
                      :item/num
                      :item/author
                      :item/category])

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

(defn unknown-feeds [urls]
  (let [known (->> (d/q '[:find ?url
                          :in $ [?url ...]
                          :where [_ :feed/url ?url]]
                        (d/db conn) urls)
                   (map first)
                   set)]
    (remove known urls)))

(defn parse-style [subj] (split subj #" " 2))

(defn set-seen [subj]
  (let [{pos :source/position
         last :feed/last-num} subj]
    (if last
      (assoc subj :source/seen (> pos last))
      subj)))

(defn adapt-source [subj]
  (let [feed (-> (:source/feed subj)
                 (update :db/id str)
                 (rename-keys {:db/id :source/feed}))]
    (-> subj
        (dissoc :source/feed)
        (rename-keys {:db/id :source/id})
        (update :source/id str)
        (merge feed)
        set-seen)))

(defn get-user-data [user-id & opts]
  (let [selected-attrs (if (some #{:selected/details} opts)
                         (conj item-list-attrs
                               {:item/feed [:db/id :feed/title]})
                         [:db/id])
        feed-attrs (cond
                     (some #{:sources/feed-details} opts) '[*]
                     (some #{:sources/feed-title} opts) [:db/id
                                                         :feed/url
                                                         :feed/title
                                                         :feed/last-num]
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
        (update :user/sources #(map adapt-source
                                    (sort-by :source/num %))))))

(defn remove-dups [subj]
  (let [rems (distinct subj)
        active (filter :source/active rems)
        to-remove (map #(assoc % :source/active false)
                       active)]
    ;; among duplicates remove inactive ones
    (remove (set to-remove) rems)))

(defn numerate [subj]
  (map-indexed #(assoc %2 :source/num %1) subj))

(defn fetch-settings [user-id]
  (ffirst
   (d/q '[:find (pull ?u [:user/styles
                          {[:source/_user :as :user/sources]
                           [:db/id
                            :source/num
                            :source/active
                            :source/filters
                            {:source/feed [:feed/url]}]}])
          :in $ ?user-id
          :where [?u :user/id ?user-id]]
        (d/db conn) user-id)))

(defn feed-match [new old]
  (= (:source/feed new) (:feed/url (:source/feed old))))

(defn strict-match [new old]
  (and (feed-match new old)
       (= (:source/filters new) (:source/filters old))))

(defn select-match [result source]
  (let [known-ids (set (keep :db/id result)) 
        candidates (remove (fn [[_ old _]]
                             (known-ids (:db/id old))) source)]
    (let [[new old type] (first (sort-by (fn [[_ old type]]
                                           [type (:source/num old)]) candidates))
          set-pos (and (:source/active new)
                       (not (:source/active old)))]
      (conj result (merge new
                          (when old {:db/id (:db/id old)})
                          (when set-pos {:source/position -15}))))))

(defn figure-deleted [old-sources new]
  (let [kept-ids (set (keep :db/id new))]
    [new (remove kept-ids (map :db/id old-sources))]))

(defn diff-sources [new-sources old-sources]
  (->> (for [new new-sources
             old old-sources]
         (if (strict-match new old)
           [new old 0]
           (if (feed-match new old)
             [new old 1]
             [new nil 2])))
       distinct
       (group-by (comp :source/num first))
       vals
       (reduce select-match [])
       (figure-deleted old-sources)))

(defn single-value [subj]
  (-> subj ffirst first second))

(defn init-positions [updates]
  (for [upd updates]
    (if-let [pos (:source/position upd)]
      (let [last-num (single-value
                      (d/q '[:find (pull ?f [:feed/last-num])
                             :in $ ?url
                             :where [?f :feed/url ?url]]
                           (d/db conn)
                           (:source/feed upd)))]
        (update upd :source/position #(+ % last-num)))
      upd)))

(defn prepare-sources-tx [updates deletes user-id]
  (concat (for [src updates]
            (-> src
                (assoc :source/user [:user/id user-id])
                (update :source/feed #(vector :feed/url %))))
          (for [id deletes] [:db/retractEntity id])))

(defn prepare-styles-tx [old-styles new-styles user-id]
  (let [old-set (set old-styles)
        new-set (set (map #(join " " %) new-styles))]
    (concat
     (for [s (difference new-set old-set)]
       [:db/add [:user/id user-id] :user/styles s])
     (for [s (difference old-set new-set)]
       [:db/retract [:user/id user-id] :user/styles s]))))

(defn update-settings! [user-id sources styles]
  (let [{old-styles :user/styles
         old-sources :user/sources} (fetch-settings user-id)
        new-sources (->> sources
                         remove-dups
                         numerate)]
    (let [[updates deletes] (diff-sources new-sources old-sources)
          src-tx-data (prepare-sources-tx (init-positions updates)
                                          deletes
                                          user-id)
          styles-tx-data (prepare-styles-tx old-styles
                                            styles
                                            user-id)]
      (d/transact conn {:tx-data (concat src-tx-data
                                         styles-tx-data)}))))

(defn update-positions! [user-id positions]
  (let [txdata (vec (for [[src pos] positions]
                      [:db/add (u/as-int src) :source/position pos]))]
    (d/transact conn {:tx-data txdata})))

(defn selected-op [op user-id id]
  [op [:user/id user-id] :user/selected id])

(defn selected-ops [id user-id]
  (if (map? id)
    (let [tempid (:item/link id)]
      [(-> id
           prepare-item-content
           (assoc :db/id tempid))
       (selected-op :db/add user-id tempid)])
    [(selected-op :db/add user-id (u/as-int id))]))

(defn content-map [ids]
  (->> ids
       (filter map?)
       (map #(vector (:item/link %) (:item/content %)))
       (filter second)
       (into {})))

(defn selected-add! [user-id ids]
  (let [cmap (content-map ids)
        res (d/transact conn {:tx-data
                              (->> (for [id ids]
                                     (selected-ops id user-id))
                                   (apply concat)
                                   vec)})]
    (doseq [[url content] cmap]
      (u/write-file (str content-dir "/"
                         (get (:tempids res) url)) content))))

(defn selected-remove! [user-id ids]
  (d/transact conn {:tx-data
                    (vec (for [id ids]
                           (selected-op :db/retract
                                        user-id
                                        (u/as-int id))))}))

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
