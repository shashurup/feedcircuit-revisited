(ns feedcircuit-revisited.main
  (:gen-class)
  (:require [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.handler :as handler]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.stat :as stat]
            [clojure.core.memoize :as memz]
            [nrepl.server :as nrepl]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.tools.logging :as log]))

(defn -main [& args]
  (when (not (empty? args))
    (log/info "Using config " (first args))
    (conf/load-from-file (first args)))
  (let [jetty-params (select-keys (conf/param) [:host :port])
        handler (handler/create)
        repl-server (nrepl/start-server :port 7888)]
    (feed/init!)
    (stat/init!)
    (content/init-cache!)
    (log/info "Running Jetty with " jetty-params)
    (run-jetty handler jetty-params)
    (nrepl/stop-server repl-server)
    (shutdown-agents)))

; === Debugging convenience functions ===

(defn _stop-auto-sync []
  (future-cancel feed/auto-sync))

(defn _run-srv []
  (conf/load-from-file "config")
  (feed/init!)
  (stat/init!)
  (content/init-cache!)
  (def _srv
    (run-jetty (handler/create)
               {:port 8080 :join? false})))

(defn _restart-srv []
  (.stop _srv)
  (def _srv
    (run-jetty (handler/create)
               {:port 8080 :join? false})))
