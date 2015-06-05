(ns hydra.test.util-test
  (:require [clojure.test :refer :all]
            [hydra.defclass.utils :refer :all]))

(deftest array-type-test
  (testing "Array types generation"
    (is (thrown? IllegalArgumentException (get-array-type String 0)))
    (is (= "[Ljava.lang.String;" (.getName (get-array-type String 1))))
    (is (= "[[Ljava.lang.String;" (.getName (get-array-type String 2))))
    (is (= "[[[Ljava.lang.String;" (.getName (get-array-type String 3))))))

(deftest symbol-to-classname-test
  (testing "Symbol to classname resolution"
    (is (thrown? IllegalArgumentException (symbol-to-classname (symbol "SuperString"))))
    (is (thrown? IllegalArgumentException (symbol-to-classname (symbol "lang.String"))))
    (is (= java.lang.String (symbol-to-classname (symbol "String"))))
    (is (= java.lang.String (symbol-to-classname (symbol "java.lang.String"))))))

(deftest array-type-for-symbol-test
  (testing "Array type for symbol"
    (is (= "[Ljava.lang.String;" (.getName (get-array-type (symbol-to-classname (symbol "String")) 1))))
    (is (= "[Ljava.lang.String;" (.getName (get-array-type (symbol-to-classname (symbol "java.lang.String")) 1))))))
