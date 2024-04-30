(defproject feedcircuit-revisited "0.30-SNAPSHOT"
  :description "Old friend Feedcircuit revisited"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [compojure "1.7.1"]
                 [joda-time/joda-time "2.9.3"] ; implicitly required by ring
                 [ring/ring-defaults "0.3.4"]
                 [ring "1.12.1"]
                 [clj-http "3.12.4"]
                 [clojure.java-time "1.4.2"]
                 [clj-commons/fs "1.6.311"]
                 [shashurup/quasi-una-fantasia "0.13"]
                 [org.clojure/core.memoize "1.1.266"]
                 [hiccup "1.0.5"]
                 [cheshire "5.13.0"]
                 [org.jsoup/jsoup "1.17.2"]
                 [com.datomic/local "1.0.277"]
                 ;; all the stuff below is for logging
                 [org.clojure/tools.logging "1.3.0"]
                 [org.slf4j/slf4j-api "2.0.13"]
                 [org.slf4j/slf4j-simple "2.0.13"]
                 [commons-io/commons-io "2.16.1"]]
  :plugins [[lein-ring "0.12.6"]]
  :ring {:handler feedcircuit-revisited.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.4.0"]]}}
  :main feedcircuit-revisited.main
  :aot [feedcircuit-revisited.main])
