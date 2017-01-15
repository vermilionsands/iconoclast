(ns iconoclast.test.example.examples
  (:require [iconoclast.defclass :refer :all]
            [iconoclast.reflect :refer :all]
            [iconoclast.definterface :as interface]))

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

(defclass AbstractSampleClass :- [:abstract :nonfinal]
  [x y]
  SampleInterface
  (abstractMethod :- [:defm :abstract] [this])
  (concreteMethod :- [:defm] [this]))

;final class
(defclass SampleClass :- [:load-ns]
  [a            :- :final                         ;Object, final, public field
   aMutable                                       ;Object, unsynchronized-mutable
   aArr         :- [:array]                       ;Object array
   aStatic      :- [:static]                      ;static field
   aStaticArray :- [:static :array]
   b            :- [int]
   bArr         :- [:array int]
   bStatic      :- [:static int]
   bStaticArray :- [:static :array int]
   c            :- [String]
   cArr         :- [{:array 2} String]
   cStatic      :- [:static String]
   cStaticArray :- [:static :array String]
   d            :- [SampleClass]
   dArr         :- [:array SampleClass]
   dStatic      :- [:static SampleClass]
   dStaticArray :- [:static :array SampleClass]
   defaultField :- [:final]
   defaultStaticField :- [:static :final]]
  :fields-mutability :mutable

  ;should be added to each constructor
  (SampleClass :- :instance-init [this] (init-set! defaultField "default"))
  ;static init block
  (SampleClass :- :static-init [] (init-set! defaultStaticField "default"))

  (SampleClass :- :init [this]) ;empty ctor
  (SampleClass :- :init [this a b c d]) ;should generate ctor accepting Object, int, String, SampleClass and setting a, b, c, d
  (SampleClass :- :init [this d c b a]) ;as above, but in reversed arguments order
  (SampleClass :- :init [this a aArr b bArr c cArr d dArr])
  (SampleClass :- :init [this dArr d cArr c bArr b aArr a])
  (SampleClass :- [:init :varargs] [this vals :- :array]
    (init-set! a (apply + vals))              ;set final field
    (init-set! aMutable (apply + vals))       ;set mutable field
    (set! b (int (apply + vals)))             ;set mutable field using normal set!
    (.callPrivateMethod this (first vals))
    (.callProtectedMethod this (first vals)))

  (SampleClass :- :init [this x :- int] (init-set! b (int x)))
  (SampleClass :- :init [this s :- String] (init-set! c s))
  (SampleClass :- :init [this c :- SampleClass] (init-set! d c))
  (SampleClass :- :init [this instances :- [:array SampleClass]] (init-set! dArr instances))

  (SampleClass :- :init [this b :- int c :- String]
    (this! ^int (* b 2)) ;call other constructor
    (init-set! c (str c c c)))

  (SampleClass :- :init [this b :- String c :- String]
    (this! ^SampleClass (identity nil)) ;identity is required since you can't add hint to nil
    (init-set! c (str b c)))

  (SampleClass :- :init [this s :- String arr :- [{:array 2} String]]
    (this! s)
    (init-set! cArr arr))

  (SampleClass :- :init [this a :- SampleClass b :- SampleClass]
    (this! ^:array ^SampleClass (into-array SampleClass [a b])))

  ;setters and getters for object, primitive, typed and selftyped instance/static fields
  (getA :- :defm [this] a)
  (getAMutable :- :defm [this] aMutable)
  (getB :- [:defm int] [this] b)
  (getC :- [:defm String] [this] c)
  (getD :- [:defm SampleClass] [this] d)
  (getAWithDot :- :defm [this] (.a this))
  (getBWithDot :- :defm [this] (.b this))
  (getCWithDot :- :defm [this] (.c this))
  (getDWithDot :- :defm [this] (.d this))
  (setB :- [:defm void] [this x :- int] (set! b x))
  (setC :- [:defm void] [this x :- String] (set! c x))
  (setD :- [:defm void] [this x :- SampleClass] (set! d x))
  (setBWithDot :- :defm [this b] (set! (.b this) b))
  (setCWithDot :- :defm [this c] (set! (.c this) c))
  (setDWithDot :- :defm [this d] (set! (.d this) d))
  (getAStatic :- [:defm :static] [] aStatic)
  (getBStatic :- [:defm :static] [] bStatic)
  (getCStatic :- [:defm :static] [] cStatic)
  (getDStatic :- [:defm :static] [] dStatic)
  (getAStaticWithDot :- [:defm :static] [] (SampleClass/aStatic))
  (getBStaticWithDot :- [:defm :static] [] (SampleClass/bStatic))
  (getCStaticWithDot :- [:defm :static] [] (SampleClass/cStatic))
  (getDStaticWithDot :- [:defm :static] [] (SampleClass/dStatic))
  (setAStatic :- [:defm :static] [x] (set! aStatic x))
  (setBStatic :- [:defm :static] [x :- int] (set! bStatic x))
  (setCStatic :- [:defm :static] [x] (set! cStatic x))
  (setDStatic :- [:defm :static] [x] (set! dStatic x))
  (setAStaticWithDot :- [:defm :static] [aStatic] (set! SampleClass/aStatic aStatic))
  (setBStaticWithDot :- [:defm :static] [bStatic] (set! SampleClass/bStatic bStatic))
  (setCStaticWithDot :- [:defm :static] [cStatic] (set! SampleClass/cStatic cStatic))
  (setDStaticWithDot :- [:defm :static] [dStatic] (set! SampleClass/dStatic dStatic))

  ;some additional methods to check arrays and signatures
  (getAArr :- [:defm :array][this] aArr)
  (getBArr :- [:defm ints] [this] bArr)
  (getCArr :- [:defm {:array 2} String] [this] cArr)
  (getDArr :- [:defm :array SampleClass][this] dArr)
  (setAArr :- [:defm void] [this x :- :array] (set! aArr x))
  (setBArr :- [:defm void] [this x :- ints] (set! dArr x))
  (setCArr :- [:defm void] [this x :- [{:array 2} String]] (set! cArr x))
  (setDArr :- [:defm void] [this ^:array ^SampleClass x] (set! dArr x))
  (varargsMethod :- [:defm :varargs] [this ^:array opts] (count opts))

  (samplePrivateMethod :- [:defm :private] [this x] x)
  (sampleProtectedMethod :- [:defm :protected] [this x] x)
  (sampleFinalProtectedMethod :- [:defm :protected :final] [this x] x)

  ;calling methods from other methods (private, protected, public)
  (callPrivateMethod :- :defm [this x] (.samplePrivateMethod this x))
  (callProtectedMethod :- :defm [this x] (.sampleProtectedMethod this x))
  (callPrivateMethodOnOther :- :defm [this other :- SampleClass x] (.samplePrivateMethod other x))
  (callProtectedMethodOnOther :- :defm [this other :- SampleClass x] (.sampleProtectedMethod other x))

  ;calls to private and protected methods using reflection should fail at runtime
  (failingPrivateCall :- :defm [this other x] (.samplePrivateMethod other x))
  (failingProtectedCall :- :defm [this other x] (.sampleProtectedMethod other x))

  (callFoo :- :defm [this] (foo))
  (callBar :- :defm [this x y] (bar x y))
  (staticCallFoo :- [:defm :static] [] (foo))
  (staticCallBar :- [:defm :static] [x y] (bar x y))
  (callCallFoo :- [:defm :static] [x :- SampleClass] (.callFoo x))
  (callCallBar :- [:defm :static] [x :- SampleClass y z] (.callBar x y z)))

(defclass NonPublicFieldsClass [aProtected :- [:final :protected]
                                ;private mutable field with automatically generated setter and getter
                                aPrivate :- [:get :set]
                                bPrivate :- [:get :set int]
                                cPrivate :- [:get :set String]
                                dPrivate :- [:get :set NonPublicFieldsClass]
                                dPrivateArr :- [:get :set :array NonPublicFieldsClass]
                                aStaticPrivate :- [:static]
                                bStaticPrivate :- [:static int]
                                cStaticPrivate :- [:static String]
                                dStaticPrivate :- [:static NonPublicFieldsClass]]
  :fields-mutability :mutable
  :fields-visibility :private

  (NonPublicFieldsClass :- :init [this])

  ;for static fields
  (getAStaticPrivate :- [:defm :static] [] (NonPublicFieldsClass/aStaticPrivate))
  (getBStaticPrivate :- [:defm :static] [] (NonPublicFieldsClass/bStaticPrivate))
  (getCStaticPrivate :- [:defm :static] [] (NonPublicFieldsClass/cStaticPrivate))
  (getDStaticPrivate :- [:defm :static] [] (NonPublicFieldsClass/dStaticPrivate))
  (setAStaticPrivate :- [:defm :static] [x] (set! NonPublicFieldsClass/aStaticPrivate x))
  (setBStaticPrivate :- [:defm :static] [x] (set! NonPublicFieldsClass/bStaticPrivate x))
  (setCStaticPrivate :- [:defm :static] [x] (set! NonPublicFieldsClass/cStaticPrivate x))
  (setDStaticPrivate :- [:defm :static] [x] (set! NonPublicFieldsClass/dStaticPrivate x))

  ;additional getters and setters, that are not generated with :get and :set
  (getAPrivateWithoutDot :- :defm [this] aPrivate)
  (getBPrivateWithoutDot :- :defm [this] bPrivate)
  (getCPrivateWithoutDot :- :defm [this] cPrivate)
  (getDPrivateWithoutDot :- :defm [this] dPrivate)
  (setAPrivateWithoutDot :- :defm [this x] (set! aPrivate x))
  (setBPrivateWithoutDot :- :defm [this x :- int] (set! bPrivate x))
  (setCPrivateWithoutDot :- :defm [this x] (set! cPrivate x))
  (setDPrivateWithoutDot :- :defm [this x] (set! dPrivate x))
  (getAStaticPrivateWithoutDot :- [:defm :static] [] aStaticPrivate)
  (getBStaticPrivateWithoutDot :- [:defm :static] [] bStaticPrivate)
  (getCStaticPrivateWithoutDot :- [:defm :static] [] cStaticPrivate)
  (getDStaticPrivateWithoutDot :- [:defm :static] [] dStaticPrivate)
  (setAStaticPrivateWithoutDot :- [:defm :static] [x] (set! aStaticPrivate x))
  (setBStaticPrivateWithoutDot :- [:defm :static] [x :- int] (set! bStaticPrivate x))
  (setCStaticPrivateWithoutDot :- [:defm :static] [x] (set! cStaticPrivate x))
  (setDStaticPrivateWithoutDot :- [:defm :static] [x] (set! dStaticPrivate x))

  ;should fail at runtime, since there's no typehint and reflector
  ;cannot access private fields
  (getFailingOtherAPrivate :- :defm [this other] (.aPrivate other))
  (getFailingOtherBPrivate :- :defm [this other] (.bPrivate other))
  (getFailingOtherCPrivate :- :defm [this other] (.cPrivate other))
  (getFailingOtherDPrivate :- :defm [this other] (.dPrivate other))

  (getOtherAPrivate :- :defm [this other :- NonPublicFieldsClass] (.aPrivate other))
  (getOtherBPrivate :- :defm [this other :- NonPublicFieldsClass] (.bPrivate other))
  (getOtherCPrivate :- :defm [this other :- NonPublicFieldsClass] (.cPrivate other))
  (getOtherDPrivate :- :defm [this other :- NonPublicFieldsClass] (.dPrivate other))
  (setOtherAPrivate :- :defm [this other :- NonPublicFieldsClass x] (set! (.aPrivate other) x))
  (setOtherCPrivate :- :defm [this other :- NonPublicFieldsClass x] (set! (.cPrivate other) x))
  (setOtherBPrivate :- :defm [this other :- NonPublicFieldsClass x] (set! (.bPrivate other) x))
  (setOtherDPrivate :- :defm [this other :- NonPublicFieldsClass x] (set! (.dPrivate other) x))

  (callSampleClass :- [:defm :static] [x :- SampleClass] (.callFoo x))
  (callSampleClass :- [:defm :static] [] (.callFoo (SampleClass.))))

(defclass DefaultCtorClass [a :- [:get]
                            b :- [:get int]
                            c :- [:get String]
                            d :- [:get DefaultCtorClass]
                            aArr :- [:get :array]
                            bArr :- [:get ints]
                            cArr :- [:get {:array 2} String]
                            dArr :- [:get :array DefaultCtorClass]
                            e :- [:public :static]]
  :fields-visibility :private)

(defclass ParentClass :- :nonfinal
  [a
   b :- :static
   c
   d :- :static
   e :- [:protected String]
   f :- [:protected :static]]
  :fields-mutability :mutable

  (ParentClass :- :init [this a] (init-set! a a))
  (foo :- :defm [this] 0)
  (bar :- :defm [this] 1)
  (bar :- :defm [this x] x)
  (protectedMethod :- [:defm :protected] [this x] x)
  (protectedStaticMethod :- [:defm :protected :static] [x] x))

(defclass ChildClass [c :- :mutable
                      d :- [:mutable :static]]
  ParentClass
  (ChildClass :- :init [this a c d]
    (super! a) ;super ctor, required since there's no noarg one
    (init-set! c c))
  (ChildClass :- :static-init [] (set! d "d"))

  (bar [this] (* 2 (super-invoke this bar))) ;call bar from parent
  (bar [this x] (* 2 (super-invoke this bar x)))

  ;get parent field
  (getA :- :defm [this] (.a this))
  (setA :- :defm [this a] (set! (.a this) a))
  (getB :- :defm [this] (ParentClass/b))
  (setB :- :defm [this b] (set! ParentClass/b b))

  ;not visible fields
  (getCParent :- :defm [this] (super-get this c))
  (setCParent :- :defm [this c] (super-set! this c (str c c)))
  (getDParent :- :defm [this] (ParentClass/d))
  (setDParent :- :defm [this d] (set! ParentClass/d d))

  ;protected fields from parent
  (getE :- :defm [this] (.e this))
  (setE :- :defm [this e] (set! (.e this) e))
  (getF :- :defm [this] (ParentClass/f))
  (setF :- :defm [this f] (set! ParentClass/f f))

  ;e won't be visible in this case when using (.e other)
  ;resort to method handles
  (getEOther :- :defm [this other :- ParentClass] (protected-get other e))
  (setEOther :- :defm [this other :- ParentClass x] (protected-set! other e x))
  ;but it will be in this one
  (getEOther :- :defm [this other :- ChildClass] (.e other))
  (setEOther :- :defm [this other :- ChildClass x] (set! (.e other) x))

  ;calling protected methods and fields from parent
  (protectedParentMethod :- :defm [this x] (.protectedMethod this x))
  (protectedParentMethod :- :defm [this ^ParentClass other x] (protected-invoke other protectedMethod x))
  (protectedParentMethod :- :defm [this ^ChildClass other x] (.protectedMethod other x))
  (protectedParentStaticMethod :- :defm [this x] (ParentClass/protectedStaticMethod x)))

(defclass ^{Deprecated true} DeprecatedClass
  [^{Deprecated true} a]
  (^{Deprecated true} DeprecatedClass :- :init [this ^{Deprecated true} arg] nil)
  (^{Deprecated true} foo :- :defm [this ^{Deprecated true} arg] nil))