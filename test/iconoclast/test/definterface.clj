(ns iconoclast.test.definterface
  (:require [clojure.test :refer :all]
            [iconoclast.test.utils.inspect :as i])
  (:import  [java.lang.reflect Modifier]
            [iconoclast.test.utils.examples SampleInterface]))

(deftest interface-signature
  (testing "interface signature"
    ;sanity check
    (is (= true (.isInterface SampleInterface)))))

(deftest interface-methods
  (testing "interface method signatures"
    (let [f (partial i/get-method-return-class SampleInterface)]
      (is (= (f "voidMethod" nil) Void/TYPE))
      (is (= (f "objectMethod" (i/sig Object)) Object))
      (is (= (f "intMethod" (i/sig (Integer/TYPE))) (Integer/TYPE)))
      (is (= (f "stringMethod" (i/sig String)) String))
      (is (= (f "selftypeMethod" (i/sig SampleInterface)) SampleInterface))
      (is (= (f "objectArrayMethod" (i/sig (i/array Object 1)))
             (i/array Object 1)))
      (is (= (f "intArrayMethod" (i/sig (i/array (Integer/TYPE) 2)))
             (i/array (Integer/TYPE) 2)))
      (is (= (f "stringArrayMethod" (i/sig (i/array String 3)))
             (i/array String 3)))
      (is (= (f "selftypeArrayMethod" (i/sig (i/array SampleInterface 1)))
             (i/array SampleInterface 1))))))
  ;varargs methods and checks
      ;(is (= (f "varargsMethod" (i/sig (i/array Object 1))) Object))
      ;(is (= (f "intVarargsMethod" (i/sig (i/array (Integer/TYPE) 1))) Object))
      ;(is (= (f "stringVarargsMethod" (i/sig (i/array String 2))) Object))
      ;(is (= (f "selftypeVarargsMethod" (i/sig (i/array SampleInterface 1))) Object))
  ;    ))
  ;(testing "varargs methods"
  ;  (let [f #(.isVarArgs (.getDeclaredMethod SampleInterface %1 %2))]
  ;  (is (= true (f "varargsMethod" (i/sig (i/array Object 1)))))
  ;  (is (= true (f "intVarargsMethod" (i/sig (i/array (Integer/TYPE) 1)))))
  ;  (is (= true (f "stringVarargsMethod" (i/sig (i/array String 2)))))
  ;  (is (= true (f "selftypeVarargsMethod" (i/sig (i/array SampleInterface 1))))))))
