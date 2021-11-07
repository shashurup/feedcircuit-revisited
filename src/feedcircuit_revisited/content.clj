(ns feedcircuit-revisited.content
  (:require [feedcircuit-revisited.jsoup :as jsoup]
            [feedcircuit-revisited.storage :as storage]
            [feedcircuit-revisited.conf :as conf]
            [hiccup.core :as hiccup]
            [me.raynes.fs :as fs]
            [clojure.zip :as zip]
            [clojure.string :as s]
            [clj-http.client :as http]
            [clojure.tools.logging :as log]))

(def ten-minutes (* 10 60 1000))

(def http-timeouts {:socket-timeout ten-minutes
                    :connection-timeout ten-minutes})

(def summary-soft-limit 1024)
(def summary-hard-limit 4096) ; induced by datomic dev-local
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

; === figure content summary ===

(defn delete-below [node]
  (let [next-node (zip/next node)]
    (if-not (zip/end? next-node)
      (delete-below (zip/remove next-node))
      node)))

(defn protuberant-node [limit root]
  (->> root
       node-seq
       (filter #(string? (zip/node %)))
       (map #(vector (count (zip/node %)) %))
       (reduce (fn [[total last] [len node]]
                 (if (> total limit)
                   [total last]
                   [(+ total len) node])))
       second))

(defn fit-raw-limit [limit node]
  (if (> (count (hiccup/html (zip/root node))) limit)
    (fit-raw-limit limit (zip/remove node))
    node))

(defmulti summarize
  "Makes a summary from an article. The size of the summary
   is calculated basing on an average paragraph size."
  class)

(defmethod summarize clojure.lang.PersistentVector [html]
  (->> html
       find-body
       remove-h1
       html-zipper
       (protuberant-node summary-soft-limit)
       delete-below
       (fit-raw-limit summary-hard-limit)
       zip/root
       children
       hiccup/html))

(defmethod summarize String [raw-html]
  (summarize (jsoup/parse-string raw-html)))

; === main interface function ===

(def unwelcome-tags #{:aside :style})

(def tags-with-href #{:a :area})
(def tags-with-src #{:img :script :iframe :embed
                     :video :audio :track :source :input})

(defn absolute-url [url base]
  (when url
    (if base
      (str (new java.net.URL (new java.net.URL base) url))
      url)))

(defn rebase-rules [base]
  [#{:a :area}             #(update-attrs % update :href absolute-url base)
   #{:img :script
     :iframe :embed
     :video :audio :track
     :source :input}       #(update-attrs % update :src absolute-url base)
   #{:form}                #(update-attrs % update :action absolute-url base)])

(defn retrieve-and-parse [url]
  (let [response (http/get url (merge {:decode-body-headers true
                                       :as :auto}
                                      http-timeouts))]
    (if (= (:content-type response) :text/html)
      (jsoup/parse-string (:body response))
      (throw (new Exception "text/html is expected")))))

(defmulti detect
  "Finds HTML element containing core content"
  (fn [x _ _] (class x)))

(defmethod detect clojure.lang.PersistentVector [html base-url hint]
  (let [base (or (get-base html) base-url)
        transform-rules (into [unwelcome-tags :delete]
                              (rebase-rules base))]
    (if-let [content-root (find-content-element html hint)]
      (->> content-root
           zip/node
           (update-html transform-rules)
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
  (let [changed (update-html (rebase-rules base-url)
                             (jsoup/parse-string subj))]
    (hiccup/html (children (find-body changed)))))

(defn calculate-size [html]
  (->> html
       jsoup/parse-string
       find-body
       text-size-recursively))
