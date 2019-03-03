(defproject ppdsp "0.1.0-SNAPSHOT"
  :description "Privacy Preserving Data Stream Perturbation: Combining Random Projection, Translation, and Additive Noise for Privacy Preserving Data Stream Mining"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [nz.ac.waikato.cms.moa/moa "2017.06"]
                 [rhizome "0.2.9"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [net.mikera/core.matrix "0.62.0"]
                 [clatrix "0.5.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [com.clojure-goes-fast/clj-memory-meter "0.1.0"]
                 [com.github.haifengl/smile-core "1.5.2"]
                 [com.github.haifengl/smile-netlib "1.5.2"]]
  :min-lein-version "2.0.0"
  :plugins [[lein-codox "0.10.3"]
            [lein-midje "3.2.1"]
            [lein-cloverage "1.0.13"]]
  :codox {:metadata {:doc/format :markdown}}
  ;; JVM PROJECT CONFIGURATION
  :main ^:skip-aot ppdsp.core
  :target-path "target/jvm/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.9.1"]]}}
  :jvm-opts ["-Xmx6g" "-server"])
