(defproject sudoku_helper "0.1.0-SNAPSHOT"
  :description "Helper for Sudoku boards"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [seesaw "1.4.4"]]
  :main ^:skip-aot sudoku-helper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:source-paths ["src"]
                   :dependencies [[org.clojure/tools.namespace "0.2.4"]]}})
