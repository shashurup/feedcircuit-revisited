(ns feedcircuit-revisited.utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

(defn as-int [subj]
  (try
    (Long/parseLong subj)
    (catch NumberFormatException _ nil)))

(defn ensure-keys-ns [ns subj]
  (into {} (for [[k v] subj]
             [(if (namespace k)
                k
                (keyword ns (name k))) v])))

(defn ensure-coll [x]
  (cond
    (coll? x) x
    x [x]))

(defn write-file [filename data]
  (let [tempfilename (str filename ".temp")]
    (with-open [w (io/writer tempfilename)]
      (binding [*out* w] (pr data)))
    (fs/rename tempfilename filename)))

(defn read-file
  ([filename]
   (read-file filename false))
  ([filename dont-check]
   (if (or dont-check (fs/exists? filename))
     (with-open [r (java.io.PushbackReader. (io/reader filename))]
       (edn/read r)))))

(defn distinct-by [f coll]
  "Similar to distinct except that duplicates of (f element) are removed"
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[v :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen (f v))
                       (recur (rest s) seen)
                       (cons v (step (rest s) (conj seen (f v)))))))
                 xs seen)))]
    (step coll #{})))
