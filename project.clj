(defproject feedcircuit-revisited "0.1.0-SNAPSHOT"
  :description "Old friend Feedcircuit revisited"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [clj-http "3.9.1"]
                 [clojure.java-time "0.3.2"]
                 [me.raynes/fs "1.4.6"]
                 [clj-template "1.0.1"]
                 [org.clojure/core.memoize "0.7.1"]
                 [crouton "0.1.2"]]
  :plugins [[lein-ring "0.12.4"]]
  :ring {:handler feedcircuit-revisited.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]]}})
