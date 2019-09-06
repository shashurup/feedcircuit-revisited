(ns feedcircuit-revisited.jsoup
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes
            Attribute Attributes Comment DataNode
            Document Element TextNode]))

(defprotocol AsClojure
  (^:private as-clojure [x] "Turn a Java class into its Clojure equivalent"))

(extend-protocol AsClojure
  Document
  (as-clojure [doc]
    (-> doc .children first as-clojure))
  Element
  (as-clojure [element]
    (into
     [(-> element .tagName keyword)
      (-> element .attributes as-clojure not-empty)]
     (->> element .childNodes (map as-clojure) (remove nil?))))
  Attributes
  (as-clojure [attrs]
    (into {} (map as-clojure attrs)))
  Attribute
  (as-clojure [attr]
    [(keyword (.getKey attr))
     (.getValue attr)])
  TextNode
  (as-clojure [text-node]
    (.getWholeText text-node))
  DataNode
  (as-clojure [data-node]
    (.getWholeData data-node))
  Comment
  (as-clojure [comment] nil))

(defn parse-string
  ([string]
   (as-clojure (Jsoup/parse string))))
