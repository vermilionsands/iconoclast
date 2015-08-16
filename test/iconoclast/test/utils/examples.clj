(ns iconoclast.test.utils.examples
  (:require [iconoclast.defclass :refer :all]
            [iconoclast.reflect :refer :all]
            [iconoclast.definterface :as interface])
  (:import [java.util List]))

(defn foo [] "foo")
(defn bar [x y] (str "bar " x " " y))

(interface/definterface SampleInterface
  (objectMethod [x])
  (^int intMethod [^int x])
  (^String stringMethod [^String x])
  (^SampleInterface selftypeMethod [^SampleInterface x])
  (^{:array 1} objectArrayMethod [^{:array 1} x])
  (^{:array 1} ^ints intArrayMethod [^{:array 2} ^int x])
  (^{:array 3} ^String stringArrayMethod [^{:array 3} ^String x])
  (^:array ^SampleInterface selftypeArrayMethod [^:array ^SampleInterface x])
  ;not yet, this would require a small change in gen-class
  ;(^:varargs varargsMethod [^:array x])
  ;(^:varargs intVarargsMethod [^ints x])
  ;(^:varargs stringVarargsMethod [^{:array 2} ^String x])
  ;(^:varargs selftypeVarargsMethod [^:array ^SampleInterface x])
  (^void voidMethod []))

(defclass ^:abstract ^:nonfinal AbstractSampleClass [x y]
  SampleInterface
  (^:defm ^:abstract abstractMethod [this])
  (^:defm concreteMethod [this]))

;final class
(defclass ^:load-ns SampleClass [a                                                         ;Object, final, public field
                       ^:mutable aMutable                                        ;Object, unsynchronized-mutable
                       ^:mutable ^:array aArr                                    ;Object array
                       ^:mutable ^:static aStatic                                ;static field
                       ^:mutable ^:static ^:array aStaticArray
                       ^:mutable ^int b
                       ^:mutable ^ints bArr
                       ^:mutable ^:static ^int bStatic
                       ^:mutable ^:static ^:array ^int bStaticArray
                       ^:mutable ^String c
                       ^:mutable ^{:array 2} ^String cArr
                       ^:mutable ^:static ^String cStatic
                       ^:mutable ^:static ^:array ^String cStaticArray
                       ^:mutable ^SampleClass d
                       ^:mutable ^:array ^SampleClass dArr
                       ^:mutable ^:static ^SampleClass dStatic
                       ^:mutable ^:static ^:array ^SampleClass dStaticArray
                       defaultField
                       ^:static defaultStaticField]
  ;should be added to each constructor
  (^:instance-init SampleClass [this] (init-set! defaultField "default"))
  ;static init block
  (^:static-init ^:static SampleClass [] (init-set! defaultStaticField "default"))

  (^:init SampleClass [this]) ;empty ctor
  (^:init SampleClass [this a b c d]) ;should generate ctor accepting Object, int, String, SampleClass and setting a, b, c, d
  (^:init SampleClass [this d c b a]) ;as above, but in reversed arguments order
  (^:init SampleClass [this a aArr b bArr c cArr d dArr])
  (^:init SampleClass [this dArr d cArr c bArr b aArr a])
  (^:init ^:varargs SampleClass [this ^:array vals]
    (init-set! a (apply + vals))              ;set final field
    (init-set! aMutable (apply + vals))       ;set mutable field
    (set! b (int (apply + vals)))             ;set mutable field using normal set!
    (.callPrivateMethod this (first vals))
    (.callProtectedMethod this (first vals)))

  (^:init SampleClass [this ^int x] (init-set! b (int x)))
  (^:init SampleClass [this ^String s] (init-set! c s))
  (^:init SampleClass [this ^SampleClass c] (init-set! d c))
  (^:init SampleClass [this ^:array ^SampleClass instances] (init-set! dArr instances))

  (^:init SampleClass [this ^int b ^String c]
    (this! ^int (* b 2)) ;call other constructor
    (init-set! c (str c c c)))

  (^:init SampleClass [this ^String b ^String c]
    (this! ^SampleClass (identity nil)) ;identity is required since you can't add hint to nil
    (init-set! c (str b c)))

  (^:init SampleClass [this ^String s ^{:array 2} ^String arr]
    (this! s)
    (init-set! cArr arr))

  (^:init SampleClass [this ^SampleClass a ^SampleClass b]
    (this! ^:array ^SampleClass (into-array SampleClass [a b])))

  ;setters and getters for object, primitive, typed and selftyped instance/static fields
  (^:defm getA [this] a)
  (^:defm getAMutable [this] aMutable)
  (^:defm ^int getB [this] b)
  (^:defm ^String getC [this] c)
  (^:defm ^SampleClass getD [this] d)
  (^:defm getAWithDot [this] (.a this))
  (^:defm getBWithDot [this] (.b this))
  (^:defm getCWithDot [this] (.c this))
  (^:defm getDWithDot [this] (.d this))
  (^:defm ^void setB [this ^int x] (set! b x))
  (^:defm ^void setC [this ^String x] (set! c x))
  (^:defm ^void setD [this ^SampleClass x] (set! d x))
  (^:defm setBWithDot [this b] (set! (.b this) b))
  (^:defm setCWithDot [this c] (set! (.c this) c))
  (^:defm setDWithDot [this d] (set! (.d this) d))
  (^:defm ^:static getAStatic [] aStatic)
  (^:defm ^:static getBStatic [] bStatic)
  (^:defm ^:static getCStatic [] cStatic)
  (^:defm ^:static getDStatic [] dStatic)
  (^:defm ^:static getAStaticWithDot [] (SampleClass/aStatic))
  (^:defm ^:static getBStaticWithDot [] (SampleClass/bStatic))
  (^:defm ^:static getCStaticWithDot [] (SampleClass/cStatic))
  (^:defm ^:static getDStaticWithDot [] (SampleClass/dStatic))
  (^:defm ^:static setAStatic [x] (set! aStatic x))
  (^:defm ^:static setBStatic [^int x] (set! bStatic x))
  (^:defm ^:static setCStatic [x] (set! cStatic x))
  (^:defm ^:static setDStatic [x] (set! dStatic x))
  (^:defm ^:static setAStaticWithDot [aStatic] (set! SampleClass/aStatic aStatic))
  (^:defm ^:static setBStaticWithDot [bStatic] (set! SampleClass/bStatic bStatic))
  (^:defm ^:static setCStaticWithDot [cStatic] (set! SampleClass/cStatic cStatic))
  (^:defm ^:static setDStaticWithDot [dStatic] (set! SampleClass/dStatic dStatic))

  ;some additional methods to check arrays and signatures
  (^:defm ^:array getAArr [this] aArr)
  (^:defm ^ints getBArr [this] bArr)
  (^:defm ^{:array 2} ^String getCArr [this] cArr)
  (^:defm ^:array ^SampleClass getDArr [this] dArr)
  (^:defm ^void setAArr [this ^:array x] (set! aArr x))
  (^:defm ^void setBArr [this ^ints x] (set! dArr x))
  (^:defm ^void setCArr [this ^{:array 2} ^String x] (set! cArr x))
  (^:defm ^void setDArr [this ^:array ^SampleClass x] (set! dArr x))
  (^:defm ^:varargs varargsMethod [this ^:array opts] (count opts))

  (^:defm ^:private samplePrivateMethod [this x] x)
  (^:defm ^:protected sampleProtectedMethod [this x] x)
  (^:defm ^:protected ^:final sampleFinalProtectedMethod [this x] x)

  ;calling methods from other methods (private, protected, public)
  (^:defm callPrivateMethod [this x] (.samplePrivateMethod this x))
  (^:defm callProtectedMethod [this x] (.sampleProtectedMethod this x))
  (^:defm callPrivateMethodOnOther [this ^SampleClass other x] (.samplePrivateMethod other x))
  (^:defm callProtectedMethodOnOther [this ^SampleClass other x] (.sampleProtectedMethod other x))

  ;calls to private and protected methods using reflection should fail at runtime
  (^:defm failingPrivateCall [this other x] (.samplePrivateMethod other x))
  (^:defm failingProtectedCall [this other x] (.sampleProtectedMethod other x))

  (^:defm callFoo [this] (foo))
  (^:defm callBar [this x y] (bar x y))
  (^:defm ^:static staticCallFoo [] (foo))
  (^:defm ^:static staticCallBar [x y] (bar x y))
  (^:defm ^:static callCallFoo [^SampleClass x] (.callFoo x))
  (^:defm ^:static callCallBar [^SampleClass x y z] (.callBar x y z)))

(defclass NonPublicFieldsClass [^:protected aProtected
                                ;private mutable field with automatically generated setter and getter
                                ^:get ^:set ^:mutable ^:private aPrivate
                                ^:get ^:set ^:mutable ^:private ^int bPrivate
                                ^:get ^:set ^:mutable ^:private ^String cPrivate
                                ^:get ^:set ^:mutable ^:private ^NonPublicFieldsClass dPrivate
                                ^:get ^:set ^:mutable ^:private ^:array ^NonPublicFieldsClass dPrivateArr
                                ^:mutable ^:private ^:static aStaticPrivate
                                ^:mutable ^:private ^:static ^int bStaticPrivate
                                ^:mutable ^:private ^:static ^String cStaticPrivate
                                ^:mutable ^:private ^:static ^NonPublicFieldsClass dStaticPrivate]
  (^:init NonPublicFieldsClass [this])

  ;for static fields
  (^:defm ^:static getAStaticPrivate [] (NonPublicFieldsClass/aStaticPrivate))
  (^:defm ^:static getBStaticPrivate [] (NonPublicFieldsClass/bStaticPrivate))
  (^:defm ^:static getCStaticPrivate [] (NonPublicFieldsClass/cStaticPrivate))
  (^:defm ^:static getDStaticPrivate [] (NonPublicFieldsClass/dStaticPrivate))
  (^:defm ^:static setAStaticPrivate [x] (set! NonPublicFieldsClass/aStaticPrivate x))
  (^:defm ^:static setBStaticPrivate [x] (set! NonPublicFieldsClass/bStaticPrivate x))
  (^:defm ^:static setCStaticPrivate [x] (set! NonPublicFieldsClass/cStaticPrivate x))
  (^:defm ^:static setDStaticPrivate [x] (set! NonPublicFieldsClass/dStaticPrivate x))

  ;additional getters and setters, that are not generated with :get and :set
  (^:defm getAPrivateWithoutDot [this] aPrivate)
  (^:defm getBPrivateWithoutDot [this] bPrivate)
  (^:defm getCPrivateWithoutDot [this] cPrivate)
  (^:defm getDPrivateWithoutDot [this] dPrivate)
  (^:defm setAPrivateWithoutDot [this x] (set! aPrivate x))
  (^:defm setBPrivateWithoutDot [this ^int x] (set! bPrivate x))
  (^:defm setCPrivateWithoutDot [this x] (set! cPrivate x))
  (^:defm setDPrivateWithoutDot [this x] (set! dPrivate x))
  (^:defm ^:static getAStaticPrivateWithoutDot [] aStaticPrivate)
  (^:defm ^:static getBStaticPrivateWithoutDot [] bStaticPrivate)
  (^:defm ^:static getCStaticPrivateWithoutDot [] cStaticPrivate)
  (^:defm ^:static getDStaticPrivateWithoutDot [] dStaticPrivate)
  (^:defm ^:static setAStaticPrivateWithoutDot [x] (set! aStaticPrivate x))
  (^:defm ^:static setBStaticPrivateWithoutDot [^int x] (set! bStaticPrivate x))
  (^:defm ^:static setCStaticPrivateWithoutDot [x] (set! cStaticPrivate x))
  (^:defm ^:static setDStaticPrivateWithoutDot [x] (set! dStaticPrivate x))

  ;should fail at runtime, since there's no typehint and reflector
  ;cannot access private fields
  (^:defm getFailingOtherAPrivate [this other] (.aPrivate other))
  (^:defm getFailingOtherBPrivate [this other] (.bPrivate other))
  (^:defm getFailingOtherCPrivate [this other] (.cPrivate other))
  (^:defm getFailingOtherDPrivate [this other] (.dPrivate other))

  (^:defm getOtherAPrivate [this ^NonPublicFieldsClass other] (.aPrivate other))
  (^:defm getOtherBPrivate [this ^NonPublicFieldsClass other] (.bPrivate other))
  (^:defm getOtherCPrivate [this ^NonPublicFieldsClass other] (.cPrivate other))
  (^:defm getOtherDPrivate [this ^NonPublicFieldsClass other] (.dPrivate other))
  (^:defm setOtherAPrivate [this ^NonPublicFieldsClass other x] (set! (.aPrivate other) x))
  (^:defm setOtherCPrivate [this ^NonPublicFieldsClass other x] (set! (.cPrivate other) x))
  (^:defm setOtherBPrivate [this ^NonPublicFieldsClass other x] (set! (.bPrivate other) x))
  (^:defm setOtherDPrivate [this ^NonPublicFieldsClass other x] (set! (.dPrivate other) x))

  (^:defm ^:static callSampleClass [^SampleClass x] (.callFoo x))
  (^:defm ^:static callSampleClass [] (.callFoo (SampleClass.))))

(defclass DefaultCtorClass [^:get ^:private a
                            ^:get ^:private ^int b
                            ^:get ^:private ^String c
                            ^:get ^:private ^DefaultCtorClass d
                            ^:get ^:private ^:array aArr
                            ^:get ^:private ^ints bArr
                            ^:get ^:private ^{:array 2} ^String cArr
                            ^:get ^:private ^:array ^DefaultCtorClass dArr
                            ^:static e])

(defclass ^:nonfinal ParentClass [^:mutable a
                                  ^:mutable ^:static b
                                  ^:mutable c
                                  ^:mutable ^:static d
                                  ^:protected ^:mutable ^String e
                                  ^:protected ^:mutable ^:static f]
  (^:init ParentClass [this a] (init-set! a a))
  (^:defm foo [this] 0)
  (^:defm bar [this] 1)
  (^:defm bar [this x] x)
  (^:defm ^:protected protectedMethod [this x] x)
  (^:defm ^:protected ^:static protectedStaticMethod [x] x))

(defclass ChildClass [^:mutable c
                      ^:mutable ^:static d]
  ParentClass
  (^:init ChildClass [this a c d]
    (super! a) ;super ctor, required since there's no noarg one
    (init-set! c c))
  (^:static-init ChildClass [] (set! d "d"))

  (bar [this] (* 2 (s-invoke this bar))) ;call bar from parent
  (bar [this x] (* 2 (s-invoke this bar x)))

  ;get parent field
  (^:defm getA [this] (.a this))
  (^:defm setA [this a] (set! (.a this) a))
  (^:defm getB [this] (ParentClass/b))
  (^:defm setB [this b] (set! ParentClass/b b))

  ;not visible fields
  (^:defm getCParent [this] (s-get this c))
  (^:defm setCParent [this c] (s-set! this c (str c c)))
  (^:defm getDParent [this] (ParentClass/d))
  (^:defm setDParent [this d] (set! ParentClass/d d))

  ;protected fields from parent
  (^:defm getE [this] (.e this))
  (^:defm setE [this e] (set! (.e this) e))
  (^:defm getF [this] (ParentClass/f))
  (^:defm setF [this f] (set! ParentClass/f f))

  ;e won't be visible in this case when using (.e other)
  ;resort to method handles
  (^:defm getEOther [this ^ParentClass other] (p-get other e))
  (^:defm setEOther [this ^ParentClass other x] (p-set! other e x))
  ;but it will be in this one
  (^:defm getEOther [this ^ChildClass other] (.e other))
  (^:defm setEOther [this ^ChildClass other x] (set! (.e other) x))

  ;calling protected methods and fields from parent
  (^:defm protectedParentMethod [this x] (.protectedMethod this x))
  (^:defm protectedParentMethod [this ^ParentClass other x] (p-invoke other protectedMethod x))
  (^:defm protectedParentMethod [this ^ChildClass other x] (.protectedMethod other x))
  (^:defm protectedParentStaticMethod [this x] (ParentClass/protectedStaticMethod x)))

(defclass ^{Deprecated true} DeprecatedClass [^{Deprecated true} a]
  (^:init ^{Deprecated true} DeprecatedClass [this ^{Deprecated true} arg] nil)
  (^:defm ^{Deprecated true} foo [this ^{Deprecated true} arg] nil))
