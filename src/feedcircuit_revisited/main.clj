(ns feedcircuit-revisited.main
  (:gen-class)
  (:require [feedcircuit-revisited.backend :as backend]
            [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.handler :as handler]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.stat :as stat]
            [clojure.core.memoize :as memz]
            [shashurup.quf.srv :as quf]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.tools.logging :as log]))

(defn -main [& args]
  (when (not (empty? args))
    (log/info "Using config " (first args))
    (conf/load-from-file (first args)))
  (let [jetty-params (select-keys (conf/param) [:host :port])
        handler (handler/create)
        quf-server (quf/start-server 7888 false)]
    (backend/init!)
    (feed/init!)
    (stat/init!)
    (log/info "Running Jetty with " jetty-params)
    (run-jetty handler jetty-params)
    (.stop quf-server)
    (shutdown-agents)))

; === Debugging convenience functions ===

(defn _stop-auto-sync []
  (future-cancel feed/auto-sync))

(defn _start-server []
  (def _srv (run-jetty (handler/create)
                       {:port 8080 :join? false})))

(defn _run []
  (conf/load-from-file "config")
  (backend/init!)
  (feed/init!)
  (stat/init!)
  (_start-server))

(defn _restart-server []
  (.stop _srv)
  (_start-server))
