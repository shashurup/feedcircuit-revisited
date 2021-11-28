(ns feedcircuit-revisited.schema)

(def item-schema [{:db/ident :item/feed
                   :db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/one
                   :db/doc "The feed this items belongs to"}

                  {:db/ident :item/source-id
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one
                   :db/doc "News item id, usually url"}

                  {:db/ident :item/feed+id
                   :db/valueType :db.type/string
                   ; :db/valueType :db.type/tuple
                   ; :db/tupleAttrs [:item/feed :item/source-id]
                   :db/cardinality :db.cardinality/one
                   :db/unique :db.unique/identity}

                  {:db/ident :item/num
                   :db/valueType :db.type/long
                   :db/cardinality :db.cardinality/one
                   :db/doc "Item order number"}

                  {:db/ident :item/feed+num
                   :db/valueType :db.type/tuple
                   :db/tupleAttrs [:item/feed :item/num]
                   :db/cardinality :db.cardinality/one}

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

                  {:db/ident :item/has-content
                   :db/valueType :db.type/boolean
                   :db/cardinality :db.cardinality/one
                   :db/doc "True when the item has content"}

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
                  ])

(def feed-schema [{:db/ident :feed/url
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

                  {:db/ident :feed/content-to-summary-ratio
                   :db/valueType :db.type/double
                   :db/cardinality :db.cardinality/one
                   :db/doc "Average content to summary ratio for feed items"}
                  ])

(def user-schema [{:db/ident :user/id
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
                  ])

(def source-schema [{:db/ident :source/user
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
                     :db/doc "Source filters"}])

(def schema (vec (concat feed-schema item-schema user-schema source-schema)))
