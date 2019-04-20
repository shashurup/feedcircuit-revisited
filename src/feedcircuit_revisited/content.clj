(ns feedcircuit-revisited.content
  (:require [crouton.html :as html]
            [clojure.zip :as zip]))

(def tag :tag)

(def children :content)

(defn html-zipper
  ([html]
   (html-zipper html nil))
  ([html branch?]
   (zip/zipper (or branch? map?)
               children
               #(assoc %1 :content (vec %2))
               html)))

(defn node-seq [zipper]
  (take-while (complement zip/end?)
              (iterate zip/next zipper)))
