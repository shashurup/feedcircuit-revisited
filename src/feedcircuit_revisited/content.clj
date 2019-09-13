(ns feedcircuit-revisited.content
  (:require [feedcircuit-revisited.jsoup :as jsoup]
            [hiccup.core :as hiccup]
            [clojure.zip :as zip]
            [clojure.string :as s]
            [clj-http.client :as http]
            [hiccup.core :as html]
            [clojure.tools.logging :as log]))

(defn http-get [url]
  (:body (http/get url {:decode-body-headers true :as :auto})))

(def minimal-summary-size 128)
(def minimal-article-size 512)

(def tag first)

(defn children [el] (nthrest el 2))

(def attrs second)

(def element? vector?)

(defn make-element [tag attrs children]
  (into [tag attrs] children))

(defn html-zipper
  ([html]
   (html-zipper element? html))
  ([pred html]
   (zip/zipper pred
               children
               #(make-element (tag %1) (attrs %1) %2)
               html)))

(defn tag-pred [pred]
  #(and (element? %) (pred (tag %))))

(def non-content-tags #{:a :head :script :style :nav
                          :aside :footer :header :svg})

(def content-element?
  (tag-pred (comp nil? non-content-tags)))

(defn node-seq [zipper]
  (take-while (complement zip/end?)
              (iterate zip/next zipper)))

(defn el-map [f zipper]
  (->> zipper
       (iterate #(zip/next (f %)))
       (take-while (complement zip/end?))
       last
       zip/root))

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
    (if (and url-attr (get (attrs element) url-attr))
      (make-element tag
                    (update (attrs element) url-attr absolute-url base)
                    (children element))
      element)))

(defn rebase-fragment [fragment base]
  (->> fragment
       html-zipper
       (el-map #(zip/edit % make-absolute base))))

(defn find-nodes [pred html]
  (->> html
       html-zipper
       node-seq
       (filter #(pred (zip/node %)))))

(defn find-tags [pred html]
  (find-nodes #(and (element? %) (pred (tag %))) html))

(defn find-body [html]
  (->> (find-tags #{:body} html)
       (map zip/node)
       first))

(defn get-base [html]
  (->> (find-tags #{:base} html)
       (map (comp :href attrs zip/node))
       first))

; === content detection core ===

(defn text-size [node]
  (->> (children node)
       (filter string?)
       (map s/trim)
       (map count)
       (reduce +)))

(defn text-size-recursively [html]
  (->> (html-zipper content-element? html)
       node-seq
       (map zip/node)
       (filter string?)
       (map s/trim)
       (map count)
       (reduce +)))

(defn text-size-in-paragraphs [node]
  (->> (children node)
       (filter #(= (tag %) :p))
       (map text-size-recursively)
       (reduce +)))

(defn find-content-element-by [html f]
  (let [winner (->> html
                    (html-zipper content-element?)
                    node-seq
                    (map zip/node)
                    (filter content-element?)
                    (map #(vector (f %) %))
                    (reduce #(max-key first %1 %2)))
        [size content-element] winner]
    (if (> size minimal-article-size)
      content-element)))

(defn find-content-element [html]
  (or (find-content-element-by html text-size-in-paragraphs)
      (find-content-element-by html text-size)))

(defn remove-h1 [html]
  (if-let [h1 (first (find-tags #{:h1} html))]
    (zip/root (zip/remove h1))
    html))

; === figure content summary ===

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

(defmulti summarize class)

(defmethod summarize clojure.lang.PersistentVector [html]
  (when-let [content-element (find-content-element html)]
    (let [paragraphs (->> (children content-element)
                          (map #(vector (text-size-recursively %) %))
                          (reduce accumulate-content-size []))
          sizes (->> paragraphs
                     (map first)
                     (filter #(> % 0)))
          min-size (minimal-size sizes)]
      (->> paragraphs
           (take-while #(< (-(second %) (first %)) min-size))
           (map #(nth % 2))
           hiccup/html))))

(defmethod summarize String [raw-html]
  (summarize (jsoup/parse-string raw-html)))

; === experimental feature - detect by content hint ===

(defn alphabetic-only [s]
  (s/replace s #"\P{IsAlphabetic}" ""))

(def text-node? (comp string? zip/node))

(defn find-biggest-text [zipper]
  (->> (node-seq zipper)
       (filter text-node?)
       (reduce #(max-key (comp count zip/node) %1 %2))))

(defn text-only [node]
  (if (string? node)
    node
    (apply str (map text-only (children node)))))

(defn find-element-containing [zipper hint]
  (let [source (alphabetic-only (text-only hint))
        size (min minimal-summary-size (count source))]
    (->> (node-seq zipper)
         (filter text-node?)
         (filter #(> (count (zip/node %)) size))
         (filter #(s/includes? source
                               (alphabetic-only (subs (zip/node %) 0 size))))
         first)))

(defn find-anchor-element [zipper hint]
  (or (if hint (find-element-containing zipper hint))
      (find-biggest-text zipper)))

(defn content-axis [html hint]
  (let [zipper (html-zipper content-element? html)
        anchor (find-anchor-element zipper hint)]
    (->> (iterate zip/up anchor)
         (map zip/node)
         (take-while #(not= :html (tag %)))
         (map #(vector (text-size-recursively %) %)))))

(defn find-content-element2 [html hint]
  (->> (content-axis html hint)
       (partition 2 1)
       (map (fn [[[l1 n1] [l2 n2]]] (vector (- l2 l1) n2)))
       (reduce #(max-key first %1 %2))
       second))

; === main interface function ===

(defn retrieve-and-parse [url]
  (jsoup/parse-string (http-get url)))

(defmulti detect (fn [x _ _] (class x)))

(defmethod detect clojure.lang.PersistentVector [html base-url hint]
  (try
    (let [base (or (get-base html) base-url)
          hint-html (if (not (empty? hint))
                      (jsoup/parse-string hint))]
      (if-let [content-root (find-content-element html)]
        (-> content-root
            (rebase-fragment base)
            remove-h1
            children
            hiccup/html)))
    (catch Exception ex
      (log/error "Failed to find content from" base-url))))

(defmethod detect String [raw-html base-url hint]
  (detect (jsoup/parse-string raw-html) base-url hint))

(defmulti get-title class)

(defmethod get-title clojure.lang.PersistentVector [html]
  (->> (find-tags #{:title} html)
       (map (comp text-only zip/node))
       first))

(defmethod get-title String [raw-html]
  (get-title (jsoup/parse-string raw-html)))

(defn make-refs-absolute [subj base-url]
  (let [changed (rebase-fragment (jsoup/parse-string subj) base-url)]
    (hiccup/html (children (find-body changed)))))

(defn calculate-size [html]
  (->> html
       jsoup/parse-string
       find-body
       text-size-recursively))
