(ns feedcircuit-revisited.schema
  (:require [clojure.set :as set]
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

(defn prepare-feed-tx-data [feed items]
  (let [feed (convert-feed feed)
        items (map convert-item items)]
    (apply concat
           (map #(into [:db/add "feed"] %) feed)
           (for [item items
                 :let [tmpid (->> item
                                  (filter #(= (first %) :item/num))
                                  first
                                  second
                                  str)]]
             (into (list [:db/add tmpid :item/feed "feed"])
                   (map #(into [:db/add tmpid] %) item))))))

(defn import-feed [db-conn feed-url]
  (let [feed  (feed/get-feed-attrs feed-url)
        items (map #(feed/fix-summary-and-content % nil) ; apply hard summary limit - 4096
                   (feed/get-numbered-items feed-url 0))]
    (d/transact db-conn {:tx-data (prepare-feed-tx-data feed items)})))
