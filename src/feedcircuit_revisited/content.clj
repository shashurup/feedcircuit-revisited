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

(defn attrs [el]
  (when (map? (second el))
    (second el)))

(defn children [el]
  (if (attrs el)
    (nthrest el 2)
    (rest el)))

(def element? vector?)

(defn make-element [tag attrs children]
  (->> (list* tag attrs children)
       (remove nil?)
       vec))

(defn update-attrs [node update-fn & params]
  (if (element? node)
    (make-element (tag node)
                  (apply update-fn (attrs node) params)
                  (children node))
    node))

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

(def non-content-tags
  "Tags where content isn't searched for"
  #{:a :head :script :style :nav
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

(defn update-html [rules html]
  (let [tr (->> rules
                (partition 2)
                (map (fn [[pred fn]]
                       [(if (set? pred)
                          #(and (element? %) (pred (tag %)))
                          pred)
                        fn])))
        update-fn (fn [loc]
                    (let [node (zip/node loc)
                          fns (->> tr
                                   (filter (fn [[pred _]] (pred node)))
                                   (map second))]
                      (cond
                        (empty? fns) loc
                        (some #(= % :delete) fns) (zip/remove loc)
                        :else (zip/edit loc (apply comp fns))
                        )))]
    (->> html
         html-zipper
         (iterate #(zip/next (update-fn %)))
         (take-while (complement zip/end?))
         last
         zip/root)))

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

(defn text-only [node]
  (if (string? node)
    node
    (apply str (map text-only (children node)))))

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

(defn get-identifier [node]
  (let [attrs (attrs node)]
    (if-let [id (:id attrs)]
      [:id id]
      (if-let [class (:class attrs)]
        [:class class]))))

(defn find-by-identifier [ident html]
  (find-nodes #(= (get-identifier %) ident) html))

; === content detection core ===

(defn text-size [zipper]
  (->> (zip/children zipper)
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

(defn text-size-in-paragraphs [zipper]
  (->> (zip/children zipper)
       (filter #(= (tag %) :p))
       (map text-size-recursively)
       (reduce +)))

(defn find-content-element-by [html f]
  (let [winner (->> html
                    (html-zipper content-element?)
                    node-seq
                    (filter zip/branch?)
                    (map #(vector (f %) %))
                    (reduce #(max-key first %1 %2)))
        [size content-element] winner]
    (if (> size minimal-article-size)
      content-element)))

(defn find-content-by-ident [html ident]
  (when ident
    (first (find-by-identifier ident html))))

(defn find-content-element
  ([html] (find-content-element html nil))
  ([html hint]
   (or (find-content-by-ident html hint)
       (find-content-element-by html text-size-in-paragraphs)
       (find-content-element-by html text-size))))

(defn remove-h1 [html]
  (if-let [h1 (first (find-tags #{:h1} html))]
    (zip/root (zip/remove h1))
    html))

(defn remove-class-and-style [el]
  (if (element? el)
    (make-element (tag el)
                  (dissoc (attrs el) :class :style)
                  (children el))
    el))

(defn neutralize [html]
 (->> html
      html-zipper
      (el-map #(zip/edit % remove-class-and-style))))

(def unwelcome-tags #{:aside})

(defn ditch-unwelcome [html]
  (->> html
       html-zipper
       (el-map #(if (unwelcome-tags (tag (zip/node %)))
                  (zip/remove %)
                  %))))

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

(defmulti summarize
  "Makes a summary from an article. The size of the summary
   is calculated basing on an average paragraph size."
  class)

(defmethod summarize clojure.lang.PersistentVector [html]
  (when-let [content-element (find-content-element html)]
    (let [paragraphs (->> content-element
                          zip/node
                          remove-h1
                          children
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

; === main interface function ===

(defn retrieve-and-parse [url]
  (jsoup/parse-string (http-get url)))

(defmulti detect
  "Finds HTML element containing core content"
  (fn [x _ _] (class x)))

(defmethod detect clojure.lang.PersistentVector [html base-url hint]
  (let [base (or (get-base html) base-url)]
    (if-let [content-root (find-content-element html hint)]
      (-> content-root
          zip/node
          ditch-unwelcome
          neutralize
          (rebase-fragment base)
          remove-h1
          children))))

(defmethod detect String [raw-html base-url hint]
  (-> raw-html
      jsoup/parse-string
      (detect base-url hint)
      hiccup/html))

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
