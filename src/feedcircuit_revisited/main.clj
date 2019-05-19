(ns feedcircuit-revisited.main
  (:gen-class)
  (:require [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.handler :as handler]
            [feedcircuit-revisited.feed :as feed]
            [clojure.core.memoize :as memz]
            [ring.adapter.jetty :refer [run-jetty]]))

(defn -main [& args]
  (if (not (empty args))
    (conf/load-from-file (first args)))
  (let [port (conf/param :port)
        handler (handler/create)]
    (feed/init!)
    (run-jetty handler {:port port})))

; === Debugging convenience functions ===

(defn _stop-auto-sync []
  (future-cancel feed/auto-sync))

(defn _drop-cache []
  (reset! feed/dir-cache {})
  (memz/memo-clear! feed/get-data))

(defn _run-srv []
  (conf/load-from-file "config")
  (feed/init!)
  (def _srv
    (run-jetty (handler/create)
               {:port 8080 :join? false})))
