(defproject kakuro "1.0-SNAPSHOT"

  :description "A simple tool for solving Kakuro puzzles, in Clojure. Only command line provided."

  :url "http://example.com/FIXME"

  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  
  :dependencies [
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [hiccup "1.0.5"]
                 ]
  
  :main ^:skip-aot kakuro.core
  :jvm-opts ["-Xmx6g" "-XX:TieredStopAtLevel=1"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
