(ns feedcircuit-revisited.content
  (:require [crouton.html :as crouton]
            [hiccup.core :as hiccup]
            [clojure.zip :as zip]
            [clojure.string :as s]
            [clj-http.client :as http]
            [hiccup.core :as html]))

(defn http-get [url]
  (:body (http/get url {:decode-body-headers true :as :auto})))

(def minimal-summary-size 128)
(def minimal-article-size 512)

(def tag :tag)

(def children :content)

(def attrs :attrs)

(def element? map?)

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

(defn el-map [f zipper]
  (->> zipper
       (iterate #(zip/next (f %)))
       (take-while (complement zip/end?))
       last
       zip/root))

(defn to-hiccup [node]
  (cond
    (map? node) (into [(:tag node) (:attrs node)] (map to-hiccup (:content node)))
    (coll? node) (map to-hiccup node)
    :else node))

(def url-attrs {:a :href
                :area :href
                :img :src
                :script :src
                :iframe :src
                :embed :src
                :video :src
                :audio :src
                :track :src
                :source :src
                :input :src
                :form :action})

(defn absolute-url [url base]
  (str (new java.net.URL (new java.net.URL base) url)))

(defn make-absolute [element base]
  (let [tag (tag element)
        url-attr (get url-attrs tag)]
    (if url-attr
      (update-in element [:attrs url-attr] absolute-url base)
      element)))

(defn rebase-fragment [fragment base]
  (->> fragment
       html-zipper
       (el-map #(zip/edit % make-absolute base))))

(defn count-paragraphs [node]
  (->> (children node)
       (filter #(= (tag %) :p))
       count))

(defn find-content-element [html]
  (let [candidates (->> html
                        html-zipper
                        node-seq
                        (map zip/node)
                        (filter element?)
                        (map #(vector (count-paragraphs %) %))
                        (sort-by first))
        [para-count content-element] (last candidates)]
    (if (> para-count 0)
      content-element)))

(defn text-only [node]
  (if (string? node)
    node
    (apply str (map text-only (children node)))))

(defn expectation [coll]
  (quot (apply + coll) (count coll)))

(defn variance [coll expectation]
  (let [sum (->> coll
                 (map #(- expectation %))
                 (map #(* % %))
                 (apply + ))]
    (-> (quot sum (count coll))
        Math/sqrt
        Math/round)))

(defn minimal-size [coll]
  (let [expectation (expectation coll)
        variance (variance coll expectation)]
    (max (- expectation variance) minimal-summary-size)))

(defn accumulate-content-size [result [size element]]
  (let [current-size (or (second (last result)) 0)]
    (conj result [size (+ size current-size) element])))

(defn summarize-raw [html]
  (when-let [content-element (find-content-element html)]
    (let [paragraphs (->> (children content-element)
                          (map #(vector (count (s/trim (text-only %))) %))
                          (reduce accumulate-content-size []))
          sizes (->> paragraphs
                     (filter #(= (tag (nth % 2)) :p))
                     (map first)
                     (filter #(> % 0)))
          min-size (minimal-size sizes)]
      (->> paragraphs
           (take-while #(< (-(second %) (first %)) min-size))
           (map #(nth % 2))))))

(defn summarize [html]
  (if-let [summary (summarize-raw (crouton/parse-string html))]
    (hiccup/html (to-hiccup summary))))

(defn detect [url]
  (let [html (crouton/parse-string (http-get url))]
    (if-let [content-root (find-content-element html)]
      (if (> (count (text-only content-root)) minimal-article-size)
        (hiccup/html (to-hiccup (children (rebase-fragment content-root url))))))))
