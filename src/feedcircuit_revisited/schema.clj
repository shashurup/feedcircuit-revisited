(ns feedcircuit-revisited.schema
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [feedcircuit-revisited.backend :as backend]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.content :as content]
            [datomic.client.api :as d]))

(def schema [{:db/ident :item/id
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "News item id, usually url"}

             {:db/ident :item/num
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one
              :db/doc "Item order number"}

             {:db/ident :item/link
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "News item link, usually the same as id"}

             {:db/ident :item/title
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "News item title"}

             {:db/ident :item/summary
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "News item content summary"}

             {:db/ident :item/content
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "News item content (if any)"}

             {:db/ident :item/author
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/many
              :db/doc "News item author(s)"}

             {:db/ident :item/contributor
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/many
              :db/doc "News item author(s)"}

             {:db/ident :item/category
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/many
              :db/doc "News item categories"}

             {:db/ident :item/published
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "News item publication time"}

             {:db/ident :item/comments
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "News item comments section link"}

             {:db/ident :item/feed
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one
              :db/doc "The feed this items belongs to"}


             {:db/ident :feed/url
              :db/unique :db.unique/identity
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "Feed url"}

             {:db/ident :feed/title
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "Feed title"}

             {:db/ident :feed/summary
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "Feed summary"}

             {:db/ident :feed/published
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "Feed publication time"}

             {:db/ident :feed/image
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "Feed icon"}


             {:db/ident :user/id
              :db/unique :db.unique/identity
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "User's id, usually e-mail"}

             {:db/ident :user/selected
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/many
              :db/doc "User's selected items"}

             {:db/ident :user/styles
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/many
              :db/doc "User's styles and scripts"}


             {:db/ident :source/user
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one
              :db/doc "User reference"}

             {:db/ident :source/feed
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one
              :db/doc "Feed reference"}

             {:db/ident :source/num
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one
              :db/doc "Source order number"}

             {:db/ident :source/active
              :db/valueType :db.type/boolean
              :db/cardinality :db.cardinality/one
              :db/doc "Used to mark a source as inactive"}

             {:db/ident :source/position
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one
              :db/doc "User's position in a source"}

             {:db/ident :source/filters
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "Source filters"}
             ])

(def feed-key-mappings {:url :feed/url
                        :title :feed/title
                        :summary :feed/summary
                        :published :feed/published
                        :image :feed/image})

(def item-key-mappings {:id :item/id
                        :num :item/num
                        :link :item/link
                        :title :item/title
                        :summary :item/summary
                        ; do not store content in datomic
                        ; :content :item/content 
                        :author :item/author
                        :contributor :item/contributor
                        :category :item/category
                        :published :item/published
                        :comments :item/comments})

(defn remove-nils [subj]
  (into {} (remove (fn [[_ v]] (nil? v)) subj)))

(defn convert-keys [subj keymap]
  (->> subj
       (filter #(keymap (first %)))
       (map (fn [[k v]] [(keymap k) v]))
       (remove #(nil? (second %)))
       (mapcat (fn [[k v]]
                 (let [vals (if (vector? v) v [v])]
                   (for [val vals] [k val]))))))

(defn convert-item [item]
  (convert-keys item item-key-mappings))

(defn convert-feed [feed]
  (convert-keys feed feed-key-mappings))

(defn prepare-feed-tx-data [feed]
  (map #(into [:db/add "feed"] %) (convert-feed feed)))

(defn prepare-items-tx-data [feed-url items]
  (->> 
   (for [item items
         :let [tmpid (str (:num item))]]
     (into (list [:db/add tmpid :item/feed [:feed/url feed-url]])
           (map #(into [:db/add tmpid] %) (convert-item item))))
   (partition-all 1024)
   (map #(apply concat %))))

(defn import-feed [db-conn feed-url]
  (let [feed  (feed/get-feed-attrs feed-url)
        items (map #(feed/fix-summary-and-content % nil) ; apply hard summary limit - 4096
                   (backend/get-numbered-items feed-url 0))]
    (concat 
     (d/transact db-conn {:tx-data (prepare-feed-tx-data feed)})
     (for [data (prepare-items-tx-data feed-url items)]
       (d/transact db-conn {:tx-data data})))))

(defn parse-feed-expr [expr]
  (let [active (not= (first expr) \#)
        [feed filters] (string/split expr #"\s+" 2)]
    [active
     (if active feed (subs feed 1))
     filters]))

(defn prepare-user-tx-data [user-attrs]
  (concat
         (list [:db/add "user" :user/id (:id user-attrs)])
         (for [[k v] (:styles user-attrs)]
           [:db/add "user" :user/styles (str k " " v)])
         (for [[num feed] (map-indexed #(vector %1 %2) (:feeds user-attrs))]
           (let [[active feed filters] (parse-feed-expr feed)
                 pos ((:positions user-attrs) feed)]
             (merge 
              {:source/user "user"
               :source/feed [:feed/url feed]
               :source/num num
               :source/active active}
              (if filters {:source/filters filters} {})
              (if pos {:source/position pos} {}))
             ))))

(defn import-user [db-conn e-mail]
  (d/transact db-conn {:tx-data (prepare-user-tx-data (feed/get-user-attrs e-mail))}))
