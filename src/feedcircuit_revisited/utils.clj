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

(defn read-file [filename]
  (if (fs/exists? filename)
    (with-open [r (java.io.PushbackReader. (io/reader filename))]
      (edn/read r))))
