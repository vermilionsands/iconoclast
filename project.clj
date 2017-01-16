(defproject iconoclast "0.2.4-SNAPSHOT"
  :description "Yet another java interop"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies      [[org.clojure/clojure "1.7.0"]]
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :profiles {:test {:aot [iconoclast.test.example.examples]}}
  :aliases {"retest" ["do" "clean" ["test"]]})