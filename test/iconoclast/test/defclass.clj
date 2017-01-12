(ns iconoclast.test.defclass
  (:require [clojure.test :refer :all]
            [iconoclast.test.utils.inspect :as i])
  (:import [java.lang.reflect Modifier]
           [iconoclast.test.example.examples AbstractSampleClass SampleClass NonPublicFieldsClass DefaultCtorClass
                                           ParentClass ChildClass SampleInterface DeprecatedClass]))

(deftest final-and-nonfinal-class
  (testing "nonfinal modifier"
    (is (= (i/get-class-modifier AbstractSampleClass (Modifier/FINAL)) 0)))
  (testing "final class (default)"
    (is (not= (i/get-class-modifier SampleClass (Modifier/FINAL)) 0))))

(deftest abstract-class
  (testing "class definition"
    (is (not= (i/get-class-modifier AbstractSampleClass (Modifier/ABSTRACT)) 0))
    (is (= (i/get-class-modifier SampleClass (Modifier/ABSTRACT)) 0)))
  (testing "method definitions"
    (is (not= (i/get-method-modifier AbstractSampleClass "abstractMethod" nil (Modifier/ABSTRACT)) 0))
    (is (= (i/get-method-modifier AbstractSampleClass "concreteMethod" nil (Modifier/ABSTRACT)) 0))))

(deftest class-fields
  (let [f (partial i/get-field-class SampleClass)]
    (testing "default field"
      (is (= (f "a") Object)))
    (testing "final field"
      (is (not= (i/get-field-modifier SampleClass "a" (Modifier/FINAL)) 0))
      (is (= (i/get-field-modifier SampleClass "aMutable" (Modifier/FINAL)) 0)))
    (testing "instance and static fields"
      (is (= (i/get-field-modifier SampleClass "a" (Modifier/STATIC)) 0))
      (is (not= (i/get-field-modifier SampleClass "aStatic" (Modifier/STATIC)) 0)))
    (testing "typed fields"
      (is (= (f "b") (Integer/TYPE)))
      (is (= (f "c") String))
      (is (= (f "d") SampleClass)))
    (testing "typed array fields"
      (is (= (f "aArr") (i/array Object 1)))
      (is (= (f "bArr") (i/array (Integer/TYPE) 1)))
      (is (= (f "cArr") (i/array String 2)))
      (is (= (f "dArr") (i/array SampleClass 1))))
    (testing "typed static fields"
      (is (= (f "bStatic") (Integer/TYPE)))
      (is (= (f "cStatic") String))
      (is (= (f "dStatic") SampleClass)))
    (testing "typed static array fields"
      (is (= (f "aStaticArray") (i/array Object 1)))
      (is (= (f "bStaticArray") (i/array (Integer/TYPE) 1)))
      (is (= (f "cStaticArray") (i/array String 1)))
      (is (= (f "dStaticArray") (i/array SampleClass 1))))
    (testing "field visibility modifiers"
      (is (not= (i/get-field-modifier SampleClass "a" (Modifier/PUBLIC)) 0))
      (is (not= (i/get-field-modifier NonPublicFieldsClass "aProtected" (Modifier/PROTECTED)) 0))
      (is (not= (i/get-field-modifier NonPublicFieldsClass "aPrivate" (Modifier/PRIVATE)) 0)))))

(deftest field-access
  (testing "access fields"
    (let [x (SampleClass.)
          y (SampleClass.)]

      (is (= (.getB x) 0))
      (is (= (.getC x) nil))
      (is (= (.getD x) nil))
      (is (= (SampleClass/getAStatic) nil))
      (is (= (SampleClass/getBStatic) 0))
      (is (= (SampleClass/getCStatic) nil))
      (is (= (SampleClass/getDStatic) nil))

      (doto x
        (.setB 1)
        (.setC "x")
        (.setD y))
      (SampleClass/setAStatic "x")
      (SampleClass/setBStatic 1)
      (SampleClass/setCStatic "x")
      (SampleClass/setDStatic y)

      (is (= (.getB x) 1))
      (is (= (.getC x) "x"))
      (is (= (.getD x) y))
      (is (= (SampleClass/getAStatic) "x"))
      (is (= (SampleClass/getBStatic) 1))
      (is (= (SampleClass/getCStatic) "x"))
      (is (= (SampleClass/getDStatic) y))))

  (testing "access using dot notation"
    (let [x (SampleClass.)
          y (SampleClass.)]
      (is (= (.getBWithDot x) 0))
      (is (= (.getCWithDot x) nil))
      (is (= (.getDWithDot x) nil))
      (is (= (SampleClass/getAStaticWithDot) "x"))
      (is (= (SampleClass/getBStaticWithDot) 1))
      (is (= (SampleClass/getCStaticWithDot) "x"))
      (is (= false (nil? (SampleClass/getDStaticWithDot))))

      (doto x
        (.setBWithDot 1)
        (.setCWithDot "x")
        (.setDWithDot y))
      (SampleClass/setAStaticWithDot "y")
      (SampleClass/setBStaticWithDot 2)
      (SampleClass/setCStaticWithDot "y")
      (SampleClass/setDStaticWithDot x)

      (is (= (.getBWithDot x) 1))
      (is (= (.getCWithDot x) "x"))
      (is (= (.getDWithDot x) y))
      (is (= (SampleClass/getAStaticWithDot) "y"))
      (is (= (SampleClass/getBStaticWithDot) 2))
      (is (= (SampleClass/getCStaticWithDot) "y"))
      (is (= (SampleClass/getDStaticWithDot) x)))))

(deftest nonpublic-field-access
  (testing "accessing private fields"
    (let [x (NonPublicFieldsClass.)
          y (NonPublicFieldsClass.)]
      (is (= (.getAPrivateWithoutDot x) nil))
      (is (= (.getBPrivateWithoutDot x) 0))
      (is (= (.getCPrivateWithoutDot x) nil))
      (is (= (.getDPrivateWithoutDot x) nil))
      (is (= (NonPublicFieldsClass/getAStaticPrivateWithoutDot) nil))
      (is (= (NonPublicFieldsClass/getBStaticPrivateWithoutDot) 0))
      (is (= (NonPublicFieldsClass/getCStaticPrivateWithoutDot) nil))
      (is (= (NonPublicFieldsClass/getDStaticPrivateWithoutDot) nil))

      (doto x
        (.setAPrivateWithoutDot "x")
        (.setBPrivateWithoutDot 1)
        (.setCPrivateWithoutDot "x")
        (.setDPrivateWithoutDot y))
      (NonPublicFieldsClass/setAStaticPrivateWithoutDot "x")
      (NonPublicFieldsClass/setBStaticPrivateWithoutDot 1)
      (NonPublicFieldsClass/setCStaticPrivateWithoutDot "x")
      (NonPublicFieldsClass/setDStaticPrivateWithoutDot y)

      (is (= (.getAPrivateWithoutDot x) "x"))
      (is (= (.getBPrivateWithoutDot x) 1))
      (is (= (.getCPrivateWithoutDot x) "x"))
      (is (= (.getDPrivateWithoutDot x) y))
      (is (= (NonPublicFieldsClass/getAStaticPrivateWithoutDot) "x"))
      (is (= (NonPublicFieldsClass/getBStaticPrivateWithoutDot) 1))
      (is (= (NonPublicFieldsClass/getCStaticPrivateWithoutDot) "x"))
      (is (= (NonPublicFieldsClass/getDStaticPrivateWithoutDot) y))))

  (testing "accessing private fields using dot notation"
    (let [x (NonPublicFieldsClass.)
          y (NonPublicFieldsClass.)]
      (is (= (.getAPrivate x) nil))
      (is (= (.getBPrivate x) 0))
      (is (= (.getCPrivate x) nil))
      (is (= (.getDPrivate x) nil))
      (is (= (NonPublicFieldsClass/getAStaticPrivate) "x"))
      (is (= (NonPublicFieldsClass/getBStaticPrivate) 1))
      (is (= (NonPublicFieldsClass/getCStaticPrivate) "x"))
      (is (= false (nil? (NonPublicFieldsClass/getDStaticPrivate))))

      (doto x
        (.setAPrivate "y")
        (.setBPrivate 2)
        (.setCPrivate "y")
        (.setDPrivate y))
      (NonPublicFieldsClass/setAStaticPrivate "y")
      (NonPublicFieldsClass/setBStaticPrivate 2)
      (NonPublicFieldsClass/setCStaticPrivate "y")
      (NonPublicFieldsClass/setDStaticPrivate y)

      (is (= (.getAPrivate x) "y"))
      (is (= (.getBPrivate x) 2))
      (is (= (.getCPrivate x) "y"))
      (is (= (.getDPrivate x) y))
      (is (= (NonPublicFieldsClass/getAStaticPrivate) "y"))
      (is (= (NonPublicFieldsClass/getBStaticPrivate) 2))
      (is (= (NonPublicFieldsClass/getCStaticPrivate) "y"))
      (is (= (NonPublicFieldsClass/getDStaticPrivate) y))))

  (testing "accessing private fields on other instance"
    (let [x (NonPublicFieldsClass.)
          y (NonPublicFieldsClass.)]
      (doto x
        (.setOtherAPrivate y "y")
        (.setOtherBPrivate y 1)
        (.setOtherCPrivate y "y")
        (.setOtherDPrivate y x))
      (is (thrown? IllegalArgumentException (.getFailingOtherAPrivate x y)))
      (is (thrown? IllegalArgumentException (.getFailingOtherBPrivate x y)))
      (is (thrown? IllegalArgumentException (.getFailingOtherCPrivate x y)))
      (is (thrown? IllegalArgumentException (.getFailingOtherDPrivate x y)))
      (is (= (.getOtherAPrivate x y) "y"))
      (is (= (.getOtherBPrivate x y) 1))
      (is (= (.getOtherCPrivate x y) "y"))
      (is (= (.getOtherDPrivate x y) x)))))

(deftest generated-setters-getters
  (testing "setters and getters signatures"
    (let [f (partial i/get-method-return-class NonPublicFieldsClass)
          g (partial i/get-method-modifier NonPublicFieldsClass)]
        (is (= (f "getDPrivateArr" nil) (i/array NonPublicFieldsClass)))
        (is (= (f "setDPrivateArr" (i/sig (i/array NonPublicFieldsClass))) (Void/TYPE)))
        (is (not= (g "getDPrivateArr" nil Modifier/PUBLIC) 0))
        (is (not= (g "setDPrivateArr" (i/sig (i/array NonPublicFieldsClass)) Modifier/PUBLIC) 0))))
  (testing "setters and getters usage"
    (let [x (NonPublicFieldsClass.)]
      (is (= (.getDPrivateArr x) nil))
      (.setDPrivateArr x (into-array NonPublicFieldsClass [(NonPublicFieldsClass.) (NonPublicFieldsClass.)]))
      (is (= (count (.getDPrivateArr x)) 2)))))

(deftest declared-methods
  (let [f (partial i/get-method-return-class SampleClass)
        g (partial i/get-method-modifier SampleClass)]
    (testing "type signatures"
      (is (= (f "getAArr" nil) (i/array Object)))
      (is (= (f "getBArr" nil) (i/array (Integer/TYPE))))
      (is (= (f "getCArr" nil) (i/array String 2)))
      (is (= (f "getDArr" nil) (i/array SampleClass)))
      (is (= (f "setAArr" (i/sig (i/array Object))) (Void/TYPE)))
      (is (= (f "setBArr" (i/sig (i/array (Integer/TYPE)))) (Void/TYPE)))
      (is (= (f "setCArr" (i/sig (i/array String 2))) (Void/TYPE)))
      (is (= (f "setDArr" (i/sig (i/array SampleClass))) (Void/TYPE))))
    (testing "varargs signature"
      (let [m (.getDeclaredMethod SampleClass "varargsMethod" (i/sig (i/array Object)))]
        (is (= false (nil? m)))
        (is (.isVarArgs m))))
    (testing "private, protected and other modifiers"
      (is (not= (g "samplePrivateMethod" (i/sig Object) (Modifier/PRIVATE)) 0))
      (is (not= (g "sampleProtectedMethod" (i/sig Object) (Modifier/PROTECTED)) 0))
      (is (not= (g "getB" nil (Modifier/PUBLIC)) 0))
      (is (not= (g "sampleFinalProtectedMethod" (i/sig Object) (Modifier/FINAL)) 0))
      (is (= (g "sampleProtectedMethod" (i/sig Object) (Modifier/FINAL)) 0))))
  (testing "calling private and protected methods"
    (let [x (SampleClass.)
          y (SampleClass.)]
      (is (= (.callProtectedMethod x 1) 1))
      (is (= (.callPrivateMethod x 2) 2))
      (is (= (.callProtectedMethodOnOther x y 3) 3))
      (is (= (.callPrivateMethodOnOther x y 4) 4))
      (is (thrown? IllegalArgumentException (.failingPrivateCall x y 5)))
      (is (thrown? IllegalArgumentException (.failingProtectedCall x y 6)))))
  (testing "calling varargs method"
    (let [x (SampleClass.)]
      (is (= (.varargsMethod x nil) 0))
      (is (= (.varargsMethod x (into-array Object [:a])) 1))
      (is (= (.varargsMethod x (into-array Object [:a :b])) 2)))))

(deftest custom-ctors
  (testing "typed default ctor signature"
    (is (= true (i/ctor-exists? DefaultCtorClass (i/sig Object (Integer/TYPE) String DefaultCtorClass
                                                        (i/array Object) (i/array (Integer/TYPE)) (i/array String 2)
                                                        (i/array DefaultCtorClass))))))
  (testing "default ctor usage"
    (let [x (DefaultCtorClass. nil 0 nil nil nil nil nil nil)
          object-arr (make-array Object 1)
          int-arr (int-array 2)
          string-arr (make-array String 3 3)
          this-arr (make-array DefaultCtorClass 4)
          y (DefaultCtorClass. :x 1 "x" x object-arr int-arr string-arr this-arr)]
      (is (= (.getA y) :x))
      (is (= (.getB y) 1))
      (is (= (.getC y) "x"))
      (is (= (.getD y) x))
      (is (= (.getAArr y) object-arr))
      (is (= (.getBArr y) int-arr))
      (is (= (.getCArr y) string-arr))
      (is (= (.getDArr y) this-arr))))
  (testing "autogenerated ctor signatures"
    (is (= true (i/ctor-exists? SampleClass (i/sig Object (i/array Object) (Integer/TYPE) (i/array (Integer/TYPE))
                                                   String (i/array String 2) SampleClass (i/array SampleClass)))))
    (is (= true (i/ctor-exists? SampleClass (i/sig (i/array SampleClass) SampleClass (i/array String 2) String (i/array (Integer/TYPE))
                                                   (Integer/TYPE) (i/array Object) Object)))))
  (testing "autogenerated ctor usage"
    (let [object-arr (make-array Object 1)
          int-arr (int-array 2)
          string-arr (make-array String 3 3)
          this-arr (make-array SampleClass 4)
          z (SampleClass.)
          x (SampleClass. :a object-arr 1 int-arr "test" string-arr z this-arr)
          y (SampleClass. this-arr z string-arr "test" int-arr 1 object-arr :a)]
      (is (= (.getA x) :a))
      (is (= (.getA y) :a))
      (is (= (.getB x) 1))
      (is (= (.getB y) 1))
      (is (= (.getC x) "test"))
      (is (= (.getC y) "test"))
      (is (= (.getD x) z))
      (is (= (.getD y) z))
      (is (= (.getAArr x) object-arr))
      (is (= (.getAArr y) object-arr))
      (is (= (.getBArr x) int-arr))
      (is (= (.getBArr y) int-arr))
      (is (= (.getCArr x) string-arr))
      (is (= (.getCArr y) string-arr))
      (is (= (.getDArr x) this-arr))
      (is (= (.getDArr y) this-arr))))
  (testing "varargs in ctors"
    (let [m (.getDeclaredConstructor SampleClass (i/sig (i/array Object)))]
      (is (= false (nil? m)))
      (is (.isVarArgs m))))
  (testing "custom ctor"
    (let [x (SampleClass. (to-array [1 2 3]))
          y (SampleClass. (make-array Object 0))]
      (is (= (.getA x) 6))
      (is (= (.getAMutable x) 6))
      (is (= (.getB x) 6))
      (is (= (.getA y) 0))
      (is (= (.getAMutable y) 0))
      (is (= (.getB y) 0))))
  (testing "this!"
    (let [string-arr (make-array String 3 3)
          x (SampleClass. (int 2) "x")
          y (SampleClass. "x" "y")
          z (SampleClass. "x" string-arr)
          v (SampleClass. x y)]
      (is (= (.getB x) 4))
      (is (= (.getC x) "xxx"))
      (is (= (.getC y) "xy"))
      (is (= (.getC z) "x"))
      (is (= (.getCArr z) string-arr))
      (is (= (first (.getDArr v)) x))))
  (testing "super!"))

(deftest init-blocks
  (testing "static inits"
    (is (= (SampleClass/defaultStaticField) "default")))
  (testing "instance inits"
    (let [x (SampleClass.)
          y (SampleClass. (to-array [1 2 3]))
          z (SampleClass. (int 2) "x")]
      (is (= (.defaultField x) "default"))
      (is (= (.defaultField y) "default"))
      (is (= (.defaultField z) "default")))))

(deftest inheritance
  (testing "class hierarchy"
    (let [x (bases AbstractSampleClass)
          y (bases ParentClass)
          z (bases ChildClass)]
      (is (= (count x) 2))
      (is (= (count y) 1))
      (is (= (count z) 1))
      (is (some #(= % Object) x))
      (is (some #(= % SampleInterface) x))
      (is (some #(= % Object) y))
      (is (some #(= % ParentClass) z))))
  (let [x (ParentClass. "a")
        y (ChildClass. "b" "c" "d")
        z (ChildClass. "bb" "cc" "dd")]
    (testing "inherited methods"
      (is (= (.foo x) 0))
      (is (= (.foo y) 0))
      (is (= (.bar x) 1))
      (is (= (.bar x 1) 1))
      (is (= (.bar y) 2))
      (is (= (.bar y 1) 2)))
    (testing "protected fields and methods"
      (is (= (.protectedParentMethod y 1) 1))
      (is (= (.protectedParentMethod y x 1) 1))
      (is (= (.protectedParentMethod y z 2) 2))
      (is (= (.protectedParentStaticMethod y 1) 1))
      (is (= (.getA y) "b"))
      (is (= (.getB y) nil))
      (is (= (.c y) "c"))
      (is (= (ChildClass/d) "d"))
      (is (= (.getCParent y) nil))
      (is (= (.getDParent y) nil))
      (is (= (.getE y) nil))
      (is (= (.getF y) nil))
      (is (= (.getEOther y x) nil))
      (is (= (.getEOther y z) nil))
      ;set some value
      (.setA y "aa")
      (.setB y "bb")
      (set! (.c y) "cc")
      (set! (ChildClass/d) "dd")
      (.setCParent y "C")
      (.setDParent y "DD")
      (.setE y "EE")
      (.setF y "FF")
      (.setEOther y x "XEE")
      (.setEOther y z "ZEE")

      (is (= (.getA y) "aa"))
      (is (= (.getB y) "bb"))
      (is (= (.c y) "cc"))
      (is (= (ChildClass/d) "dd"))
      (is (= (.getCParent y) "CC"))
      (is (= (.getDParent y) "DD"))
      (is (= (.getE y) "EE"))
      (is (= (.getF y) "FF"))
      (is (= (.getEOther y x) "XEE"))
      (is (= (.getEOther y z) "ZEE")))))

(deftest annotations
  (testing "class annotations"
    (is (not= (.getAnnotation DeprecatedClass java.lang.Deprecated) nil)))
  (testing "field annotations"
    (is (not= (.getAnnotation (.getField DeprecatedClass "a") java.lang.Deprecated) nil)))
  (testing "method annotations"
    (is (not= (.getAnnotation (.getMethod DeprecatedClass "foo" (i/sig Object)) java.lang.Deprecated) nil)))
  (testing "ctor annotations"
    (is (not= (.getAnnotation (.getConstructor DeprecatedClass (i/sig Object)) java.lang.Deprecated) nil)))
  (testing "method arg annotations"
    (is (= (.annotationType (aget (.getParameterAnnotations (.getMethod DeprecatedClass "foo" (i/sig Object))) 0 0))
           java.lang.Deprecated)))
  (testing "ctor arg annotations"
    (is (= (.annotationType (aget (.getParameterAnnotations (.getConstructor DeprecatedClass (i/sig Object))) 0 0))
           java.lang.Deprecated))))

(deftest function-calls
  (testing "calling fuctions from outside"
    (let [x (SampleClass.)]
      (is (= (.callFoo x) "foo"))
      (is (= (SampleClass/staticCallFoo) "foo"))
      (is (= (SampleClass/callCallFoo x) "foo"))
      (is (= (.callBar x "a" "b") "bar a b"))
      (is (= (SampleClass/staticCallBar "a" "b") "bar a b"))
      (is (= (SampleClass/callCallBar x "a" "b") "bar a b"))
      (is (= (NonPublicFieldsClass/callSampleClass x) "foo"))
      (is (= (NonPublicFieldsClass/callSampleClass) "foo")))))
