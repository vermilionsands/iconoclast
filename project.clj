(defproject hydra "0.1.0"
  :description "Slightly modified Clojure compiler"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies      [[org.clojure/clojure "1.6.0"]]
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :profiles {:test {:aot [hydra.test.examples]}})
