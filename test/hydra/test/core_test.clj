(ns hydra.test.core-test
  (:require [clojure.test :refer :all]
            [hydra.defclass :refer :all])
  (:import [java.lang.reflect Modifier]
           [hydra.test.examples TestNonFinalClass TestFinalClass TestTypedFields TestDeclaredMethods
                                TestAbstractClass TestProtectedClass TestPublicClass TestStaticsClass
                                ParentClass ChildClass]))
;add tests for this fns
(defn is-class-modifier? [c m]
  (not (= 0 (bit-and m (.getModifiers c)))))

(defn is-field-class? [c field-name field-class]
  (= field-class (.getType (.getDeclaredField c field-name))))

(defn is-field-classname? [c field-name field-classname]
  (= field-classname (.getName (.getType (.getDeclaredField c field-name)))))

(deftest test-final-nonfinal-class
  (testing "Final and nonfinal meta"
    (is (not (is-class-modifier? TestNonFinalClass (Modifier/FINAL))))
    (is (is-class-modifier? TestFinalClass (Modifier/FINAL)))))

(deftest test-typed-field
  (testing "Typed fields definitions"
    (is (is-field-class? TestTypedFields "alpha" java.lang.Object))
    (is (is-field-class? TestTypedFields "beta" java.lang.String))
    (is (is-field-class? TestTypedFields "gamma" java.util.List))
    (is (is-field-class? TestTypedFields "delta" java.util.HashMap))
    (is (is-field-classname? TestTypedFields "epsilon" "int"))
    (is (is-field-classname? TestTypedFields "theta" "[I"))
    (is (is-field-classname? TestTypedFields "iota" "[Ljava.lang.String;"))
    (is (is-field-classname? TestTypedFields "zeta" "[[[Ljava.lang.Long;")))
  (testing "Typed fields usage"
    (let [x (TestTypedFields. nil nil nil nil 0 nil nil nil)
          l (java.util.ArrayList.)
          m (java.util.HashMap.)
          a (int-array 3 [1 2 3])
          string-arr-1 (make-array String 2)
          string-arr-2 (make-array String 2 2)
          long-arr (make-array Long 2 2 2)]
      (doto x
        (.setAlpha "alpha")
        (.setBeta "beta")
        (.setGamma l)
        (.setDelta m)
        (.setEpsilon 1)
        (.setTheta a)
        (.setIota string-arr-1)
        (.setZeta long-arr))
      (is (= "beta" (.getBeta x)))
      (is (= l (.getGamma x)))
      (is (= m (.getDelta x)))
      (is (= 1 (.getEpsilon x)))
      (is (= a (.getTheta x)))
      (is (= string-arr-1 (.getIota x)))
      (is (thrown? ClassCastException (.setIota x string-arr-2)))
      (is (= long-arr (.getZeta x))))))

(deftest test-declared-methods
  (testing "Declared signatures"
    (let [a (.getDeclaredMethod TestDeclaredMethods "getAlpha" nil)
          b (.getDeclaredMethod TestDeclaredMethods "getBeta" nil)
          c (.getDeclaredMethod TestDeclaredMethods "getGamma" nil)
          d (.getDeclaredMethod TestDeclaredMethods "setBeta" (into-array Class [java.lang.String]))
          e (.getDeclaredMethod TestDeclaredMethods "setGamma" (into-array Class [Integer/TYPE]))]
      (is (= java.lang.Object (.getReturnType a)))
      (is (= java.lang.String (.getReturnType b)))
      (is (= "int" (.getName (.getReturnType c))))
      (is (= "void" (.getName (.getReturnType d))))
      (is (= "void" (.getName (.getReturnType e))))))
  (testing "Declared methods"
    (let [x (TestDeclaredMethods. 1.0 "Test" 2)]
      (is (= (.getAlpha x) 1.0))
      (is (= (.getBeta x) "Test"))
      (is (= (.getGamma x) 2))
      (doto x
        (.setBeta "Changed")
        (.setGamma 3))
      (is (= (.getBeta x) "Changed"))
      (is (= (.getGamma x) 3)))))

(deftest test-modifiers
  (testing "Class modifiers"
    (is (= (Modifier/isAbstract (.getModifiers TestAbstractClass))))
    (is (= (Modifier/isPublic (.getModifiers TestAbstractClass))))
    (is (= (not (Modifier/isFinal (.getModifiers TestAbstractClass)))))
    (is (= (Modifier/isProtected (.getModifiers TestProtectedClass))))
    (is (= (Modifier/isFinal (.getModifiers TestProtectedClass))))
    (is (= (Modifier/isPublic (.getModifiers TestPublicClass)))))
  (testing "Field modifiers"
    (is (= (Modifier/isPublic (.getModifiers (.getDeclaredField TestPublicClass "alpha")))))
    (is (= (not (Modifier/isFinal (.getModifiers (.getDeclaredField TestPublicClass "alpha"))))))
    (is (= (Modifier/isPrivate (.getModifiers (.getDeclaredField TestPublicClass "beta")))))
    (is (= (Modifier/isFinal (.getModifiers (.getDeclaredField TestPublicClass "beta")))))
    (is (= (Modifier/isProtected (.getModifiers (.getDeclaredField TestPublicClass "gamma")))))
    (is (= (Modifier/isPublic (.getModifiers (.getDeclaredField TestPublicClass "delta")))))
    (is (= (Modifier/isFinal (.getModifiers (.getDeclaredField TestPublicClass "delta"))))))
  (testing "Method modifiers"
    (is (= (Modifier/isAbstract (.getModifiers (.getDeclaredMethod TestAbstractClass "foo1" nil)))))))

(deftest test-statics
  (testing "Static fields and methods"
    (is (= (not (Modifier/isStatic (.getModifiers (.getDeclaredField TestStaticsClass "alpha"))))))
    (is (= (Modifier/isStatic (.getModifiers (.getDeclaredField TestStaticsClass "beta")))))
    (let [x (TestStaticsClass. 0)
          y (TestStaticsClass. 1)]
      (TestStaticsClass/setBeta "x")
      (TestStaticsClass/setGamma 2)
      (is (= "x" (TestStaticsClass/getBeta)))
      (is (= 2 (TestStaticsClass/getGamma)))
      (is (= "x" (.getBetaFromInstance x)))
      (.setBetaFromInstance x "y")
      (is (= "y" (.getBetaFromInstance y))))))

(deftest test-ctors
  (testing "Ctors signatures"
    (let [a (.getDeclaredConstructor ParentClass nil)
          b (.getDeclaredConstructor ParentClass (into-array Class [java.lang.String java.lang.Long]))
          c (.getDeclaredConstructor ParentClass (into-array Class [java.lang.String]))
          d (.getDeclaredConstructor ParentClass (into-array Class [java.lang.Long]))]
      (is (= (count (.getDeclaredConstructors ParentClass)) 4))))
  (testing "Ctors logic"
    (let [x (ChildClass. "test")
          y (ChildClass. "a" 1 "b")
          z (ChildClass/getInstance)
          a (ParentClass.)
          b (ParentClass. "a" 1)]
      (is (= (.alpha x) "test"))
      (is (= (.beta x) 4))
      (is (= (.gamma x) "test"))
      (is (= (.alpha y) nil))
      (is (= (.beta y) 1))
      (is (= (.gamma y) "b"))
      (is (= (.alpha a) nil))
      (is (= (.beta a) nil))
      (is (= (.alpha b) "a"))
      (is (= (.beta b) 1)))))
