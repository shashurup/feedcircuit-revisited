(defproject feedcircuit-revisited "0.4.1"
  :description "Old friend Feedcircuit revisited"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [ring "1.7.1"]
                 [clj-http "3.9.1"]
                 [clojure.java-time "0.3.2"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/core.memoize "0.7.1"]
                 [hiccup "1.0.5"]
                 [org.clojure/tools.logging "0.4.1"]
                 [cheshire "5.8.1"]
                 [org.jsoup/jsoup "1.12.1"]]
  :plugins [[lein-ring "0.12.4"]]
  :ring {:handler feedcircuit-revisited.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]]}}
  :main feedcircuit-revisited.main
  :aot [feedcircuit-revisited.main])
