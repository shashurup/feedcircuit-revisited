(defproject feedcircuit-revisited "0.33"
  :description "Old friend Feedcircuit revisited"
  :url "http://feedcircuit.kibardin.name"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [compojure "1.7.1"]
                 [joda-time/joda-time "2.9.3"] ; implicitly required by ring
                 [ring/ring-defaults "0.3.4"]
                 [ring "1.12.1"]
                 [clj-http "3.12.4"]
                 [clojure.java-time "1.4.2"]
                 [clj-commons/fs "1.6.311"]
                 [shashurup/quasi-una-fantasia "0.15"]
                 [org.clojure/core.memoize "1.1.266"]
                 [hiccup "1.0.5"]
                 [cheshire "5.13.0"]
                 [org.jsoup/jsoup "1.17.2"]
                 [com.datomic/local "1.0.277"]
                 [datalevin "0.9.5"]
                 [org.clojure/tools.logging "1.3.0"]
                 [org.slf4j/slf4j-jdk14 "2.0.13"] ;; for slf4j reliant libs
                 [commons-io/commons-io "2.16.1"]]
  :plugins [[lein-ring "0.12.6"]]
  :ring {:handler feedcircuit-revisited.handler/app}
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring/ring-mock "0.4.0"]]}}
  :main feedcircuit-revisited.main
  :aot [feedcircuit-revisited.main]
  :jvm-opts ["-Dclojure.tools.logging.factory=clojure.tools.logging.impl/jul-factory"
             "-Djava.util.logging.SimpleFormatter.format=\"%1$tF %1$tT %4$s: %5$s%6$s%n\""
             "--add-opens=java.base/java.nio=ALL-UNNAMED"
             "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"]
  )
