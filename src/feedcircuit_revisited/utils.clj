(ns feedcircuit-revisited.utils)

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
