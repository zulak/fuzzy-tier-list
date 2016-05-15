(defproject fuzzy-tier-list "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/zulak/fuzzy-tier-list"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [cheshire "5.6.1"]
                 [clojure-lanterna "0.9.4"]]
  :main fuzzy-tier-list.core
  :jvm-opts ["-Xmx1g"]
  :aot [fuzzy-tier-list.core])
