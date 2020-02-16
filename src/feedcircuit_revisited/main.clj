(ns feedcircuit-revisited.main
  (:gen-class)
  (:require [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.handler :as handler]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.stat :as stat]
            [clojure.core.memoize :as memz]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.tools.logging :as log]))

(defn -main [& args]
  (when (not (empty? args))
    (log/info "Using config " (first args))
    (conf/load-from-file (first args)))
  (let [jetty-params (select-keys (conf/param) [:host :port])
        handler (handler/create)]
    (feed/init!)
    (stat/init!)
    (log/info "Running Jetty with " jetty-params)
    (run-jetty handler jetty-params)))

; === Debugging convenience functions ===

(defn _stop-auto-sync []
  (future-cancel feed/auto-sync))

(defn _drop-cache []
  (reset! feed/dir-cache {})
  (memz/memo-clear! feed/get-data))

(defn _run-srv []
  (conf/load-from-file "config")
  (feed/init!)
  (stat/init!)
  (def _srv
    (run-jetty (handler/create)
               {:port 8080 :join? false})))
