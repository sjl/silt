(defproject silt "0.4.0"
  :description "You are the god of a toroidal world."
  :url "http://bitbucket.org/sjl/silt"
  :license {:name "MIT/X11"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clojure-lanterna "0.9.5"]
                 [roul "0.2.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :main ^:skip-aot silt.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
