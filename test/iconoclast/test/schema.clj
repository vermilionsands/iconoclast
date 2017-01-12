(ns iconoclast.test.schema
  (:require [clojure.test :refer :all]
            [iconoclast.other.schema :as schema]))

(deftest schema-parse-test
  (testing "single keyword schema"
    (let [[x] (schema/process-arrow-schematized-args ['q :- :private])]
      (is (:private (meta x)))))
  (testing "single symbol schema"
    (let [[x] (schema/process-arrow-schematized-args ['q :- 'String])]
      (is (= (:tag (meta x)) 'String))))
  (testing "vector schema"
    (let [[x] (schema/process-arrow-schematized-args ['q :- [:private :static 'String]])]
      (is (:private (meta x)))
      (is (:static (meta x)))
      (is (= (:tag (meta x)) 'String)))))