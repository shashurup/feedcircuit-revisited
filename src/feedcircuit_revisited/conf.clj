(ns feedcircuit-revisited.conf
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def data {:port 8080
           :data-dir "data"
           :cookie-key "veery secret key"})

(defn param [& keys]
  (get-in data keys))

(defn load-from-file [filename]
  (def data
    (with-open [r (java.io.PushbackReader. (io/reader filename))]
      (edn/read r))))
