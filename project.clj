(defproject feedcircuit-revisited "0.19.1"
  :description "Old friend Feedcircuit revisited"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [compojure "1.6.2"]
                 [joda-time/joda-time "2.9.3"] ; implicitly required by ring
                 [ring/ring-defaults "0.3.3"]
                 [ring "1.9.4"]
                 [clj-http "3.12.3"]
                 [clojure.java-time "0.3.3"]
                 [me.raynes/fs "1.4.6"]
                 [nrepl "0.8.3"]
                 [org.clojure/core.memoize "1.0.250"]
                 [hiccup "1.0.5"]
                 [org.clojure/tools.logging "1.1.0"]
                 [cheshire "5.10.0"]
                 [org.jsoup/jsoup "1.14.3"]]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler feedcircuit-revisited.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]]}}
  :main feedcircuit-revisited.main
  :aot [feedcircuit-revisited.main])
