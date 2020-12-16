(defproject feedcircuit-revisited "0.12-SNAPSHOT"
  :description "Old friend Feedcircuit revisited"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [ring "1.8.0"]
                 [clj-http "3.10.0"]
                 [clojure.java-time "0.3.2"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/core.memoize "0.8.2"]
                 [hiccup "1.0.5"]
                 [org.clojure/tools.logging "0.5.0"]
                 [cheshire "5.10.0"]
                 [org.jsoup/jsoup "1.12.1"]]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler feedcircuit-revisited.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]]}}
  :main feedcircuit-revisited.main
  :aot [feedcircuit-revisited.main])
