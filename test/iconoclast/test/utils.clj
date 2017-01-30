(ns iconoclast.test.utils
  (:require [clojure.test :refer :all]
            [iconoclast.other.utils :refer :all]))

(deftest symbol-to-class
  (testing "Symbol to classname resolution"
    (is (thrown? IllegalArgumentException (resolve-tag (symbol "SuperString"))))
    (is (thrown? IllegalArgumentException (resolve-tag (symbol "lang.String"))))
    (is (= java.lang.String (resolve-tag (symbol "String"))))
    (is (= java.lang.String (resolve-tag "String")))
    (is (= java.lang.String (resolve-tag (symbol "java.lang.String"))))
    (is (= java.lang.String (resolve-tag "java.lang.String")))
    (is (= Integer/TYPE (resolve-tag (symbol "int"))))
    (is (= Integer/TYPE (resolve-tag "int")))))

(deftest array-type
  (testing "Array types generation"
    (is (thrown? IllegalArgumentException (get-array-type String 0)))
    (is (= "[Ljava.lang.String;" (.getName (get-array-type String 1))))
    (is (= "[[Ljava.lang.String;" (.getName (get-array-type String 2))))
    (is (= "[[[Ljava.lang.String;" (.getName (get-array-type String 3))))))

(deftest meta-mutable-to-unsychronized-test
  (testing "Conversion of :mutable meta to :unsynchronized-mutable"
    (let [m (meta-mutable-to-unsynchronized {:mutable true})]
      (is (= (nil? (:mutable m)) true))
      (is (= (:unsynchronized-mutable m) true)))))

(deftest meta-self-hint-test
  (testing "Conversion of tag value"
    (let [classname (symbol "bar.Foo")
          f (partial meta-self-hint classname)
          m1 (f {:tag (symbol "Wee")})
          m2 (f {:tag (symbol "Foo")})]
      (is (= (:tag m1) (symbol "Wee")))
      (is (= (:tag m2) (symbol "bar.Foo"))))))

(deftest meta-arr-to-hint-test
  (testing "Conversion of array and tag meta"
    (let [classname (symbol "bar.Zoo")
          f (partial meta-arr-to-hint classname)
          m1 (f {:tag String})
          m2 (f {:array true})
          m3 (f {:array 2})
          m4 (f {:array 3 :tag Long})
          m5 (f {:array true :tag (symbol "bar.Zoo")})]
      (is (= (:tag m1) String))
      (is (= (:tag m2) (symbol "[Ljava.lang.Object;")))
      (is (= (:tag m3) (symbol "[[Ljava.lang.Object;")))
      (is (= (:tag m4) (symbol "[[[Ljava.lang.Long;")))
      (is (= (:tag m5) (symbol "[Lbar.Zoo;"))))))
