(ns feedcircuit-revisited.jsoup
  (:require [clojure.string :as s])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes
            Attribute Attributes Comment DataNode
            Document Element TextNode]))

(defprotocol AsClojure
  (^:private as-clojure [x] "Turn a Java class into its Clojure equivalent"))

(defn escape-html [subj]
  (-> subj
      (s/replace "&" "&amp;")
      (s/replace "<" "&lt;")
      (s/replace ">" "&gt;")))

(extend-protocol AsClojure
  Document
  (as-clojure [doc]
    (-> doc .children first as-clojure))

  Element
  (as-clojure [element]
    (let [tag (-> element .tagName keyword)
          attrs (-> element .attributes as-clojure not-empty)]
      (into
       (if attrs [tag attrs] [tag])
       (->> element .childNodes (map as-clojure) (remove nil?)))))

  Attributes
  (as-clojure [attrs]
    (into {} (map as-clojure attrs)))

  Attribute
  (as-clojure [attr]
    [(keyword (.getKey attr))
     (.getValue attr)])

  TextNode
  (as-clojure [text-node]
    (escape-html (.getWholeText text-node)))

  DataNode
  (as-clojure [data-node]
    (.getWholeData data-node))

  Comment
  (as-clojure [comment] nil))

(defn parse-string
  ([string]
   (as-clojure (Jsoup/parse string))))
