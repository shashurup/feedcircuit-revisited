(defproject feedcircuit-revisited "0.20.1"
  :description "Old friend Feedcircuit revisited"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :repositories [["cognitect-dev-tools" {:url "https://dev-tools.cognitect.com/maven/releases/"}]]
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
                 [org.jsoup/jsoup "1.14.3"]
                 [com.datomic/dev-local "1.0.238"]]
  :plugins [[lein-ring "0.12.5"]
            [org.openjfx/javafx-maven-plugin "0.0.8"]]
  :ring {:handler feedcircuit-revisited.handler/app}
  ;:pom-addition ([:properties [:javafx.platform "linux"]])
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]
                        [com.cognitect/rebl "0.9.242"]]
         ; use system's jfx
         :resource-paths ["/usr/lib/jvm/java-11-openjfx/lib/javafx.web.jar"
                          "/usr/lib/jvm/java-11-openjfx/lib/javafx.base.jar"
                          "/usr/lib/jvm/java-11-openjfx/lib/javafx.controls.jar"
                          "/usr/lib/jvm/java-11-openjfx/lib/javafx.fxml.jar"
                          "/usr/lib/jvm/java-11-openjfx/lib/javafx.graphics.jar"
                          "/usr/lib/jvm/java-11-openjfx/lib/javafx.media.jar"
                          "/usr/lib/jvm/java-11-openjfx/lib/javafx.swing.jar"
                          "/usr/lib/jvm/java-11-openjfx/lib/javafx-swt.jar"]
         }}
  :jvm-opts ["-Dglass.gtk.uiScale=120%"] ; Scaling for REBL UI
  :main feedcircuit-revisited.main
  :aot [feedcircuit-revisited.main])
