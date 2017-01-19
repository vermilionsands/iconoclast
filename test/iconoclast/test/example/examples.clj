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
  (abstractMethod :- [:declare :abstract] [this])
  (concreteMethod :- [:declare] [this]))

;final class
(defclass SampleClass
  [a            :- :final            ;Object, final, public field
   aMutable                          ;Object, unsynchronized-mutable
   aArr         :- :array            ;Object array
   aStatic      :- :static           ;static field
   aStaticArray :- [:static :array]
   b            :- int
   bArr         :- [:array int]
   bStatic      :- [:static int]
   bStaticArray :- [:static :array int]
   c            :- String
   cArr         :- [{:array 2} String]
   cStatic      :- [:static String]
   cStaticArray :- [:static :array String]
   d            :- SampleClass
   dArr         :- [:array SampleClass]
   dStatic      :- [:static SampleClass]
   dStaticArray :- [:static :array SampleClass]
   defaultField :- :final
   defaultStaticField :- [:static :final]]

  :load-ns true
  :fields-mutability :mutable
  :method-declaration-mode :declare

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
  (getA [this] a)
  (getAMutable [this] aMutable)
  (getB :- int [this] b)
  (getC :- String [this] c)
  (getD :- SampleClass [this] d)
  (getAWithDot [this] (.a this))
  (getBWithDot [this] (.b this))
  (getCWithDot [this] (.c this))
  (getDWithDot [this] (.d this))
  (setB :- void [this x :- int] (set! b x))
  (setC :- void [this x :- String] (set! c x))
  (setD :- void [this x :- SampleClass] (set! d x))
  (setBWithDot [this b] (set! (.b this) b))
  (setCWithDot [this c] (set! (.c this) c))
  (setDWithDot [this d] (set! (.d this) d))
  (getAStatic :- :static [] aStatic)
  (getBStatic :- :static [] bStatic)
  (getCStatic :- :static [] cStatic)
  (getDStatic :- :static [] dStatic)
  (getAStaticWithDot :- :static [] (SampleClass/aStatic))
  (getBStaticWithDot :- :static [] (SampleClass/bStatic))
  (getCStaticWithDot :- :static [] (SampleClass/cStatic))
  (getDStaticWithDot :- :static [] (SampleClass/dStatic))
  (setAStatic :- :static [x] (set! aStatic x))
  (setBStatic :- :static [x :- int] (set! bStatic x))
  (setCStatic :- :static [x] (set! cStatic x))
  (setDStatic :- :static [x] (set! dStatic x))
  (setAStaticWithDot :- :static [aStatic] (set! SampleClass/aStatic aStatic))
  (setBStaticWithDot :- :static [bStatic] (set! SampleClass/bStatic bStatic))
  (setCStaticWithDot :- :static [cStatic] (set! SampleClass/cStatic cStatic))
  (setDStaticWithDot :- :static [dStatic] (set! SampleClass/dStatic dStatic))

  ;some additional methods to check arrays and signatures
  (getAArr :- :array[this] aArr)
  (getBArr :- ints [this] bArr)
  (getCArr :- [{:array 2} String] [this] cArr)
  (getDArr :- [:array SampleClass] [this] dArr)
  (setAArr :- void [this x :- :array] (set! aArr x))
  (setBArr :- void [this x :- ints] (set! bArr x))
  (setCArr :- void [this x :- [{:array 2} String]] (set! cArr x))
  (setDArr :- void [this x :- [:array SampleClass]] (set! dArr x))
  (varargsMethod :- :varargs [this ^:array opts] (count opts))

  (samplePrivateMethod :- :private [this x] x)
  (sampleProtectedMethod :- :protected [this x] x)
  (sampleFinalProtectedMethod :- [:protected :final] [this x] x)

  ;calling methods from other methods (private, protected, public)
  (callPrivateMethod [this x] (.samplePrivateMethod this x))
  (callProtectedMethod [this x] (.sampleProtectedMethod this x))
  (callPrivateMethodOnOther [this other :- SampleClass x] (.samplePrivateMethod other x))
  (callProtectedMethodOnOther [this other :- SampleClass x] (.sampleProtectedMethod other x))

  ;calls to private and protected methods using reflection should fail at runtime
  (failingPrivateCall [this other x] (.samplePrivateMethod other x))
  (failingProtectedCall [this other x] (.sampleProtectedMethod other x))

  (callFoo [this] (foo))
  (callBar [this x y] (bar x y))
  (staticCallFoo :- :static [] (foo))
  (staticCallBar :- :static [x y] (bar x y))
  (callCallFoo :- :static [x :- SampleClass] (.callFoo x))
  (callCallBar :- :static [x :- SampleClass y z] (.callBar x y z))

  (getSelf :- SampleClass [this] this))

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
  :method-declaration-mode :declare

  (NonPublicFieldsClass :- :init [this])

  ;for static fields
  (getAStaticPrivate :- :static [] (NonPublicFieldsClass/aStaticPrivate))
  (getBStaticPrivate :- :static [] (NonPublicFieldsClass/bStaticPrivate))
  (getCStaticPrivate :- :static [] (NonPublicFieldsClass/cStaticPrivate))
  (getDStaticPrivate :- :static [] (NonPublicFieldsClass/dStaticPrivate))
  (setAStaticPrivate :- :static [x] (set! NonPublicFieldsClass/aStaticPrivate x))
  (setBStaticPrivate :- :static [x] (set! NonPublicFieldsClass/bStaticPrivate x))
  (setCStaticPrivate :- :static [x] (set! NonPublicFieldsClass/cStaticPrivate x))
  (setDStaticPrivate :- :static [x] (set! NonPublicFieldsClass/dStaticPrivate x))

  ;additional getters and setters, that are not generated with :get and :set
  (getAPrivateWithoutDot [this] aPrivate)
  (getBPrivateWithoutDot [this] bPrivate)
  (getCPrivateWithoutDot [this] cPrivate)
  (getDPrivateWithoutDot [this] dPrivate)
  (setAPrivateWithoutDot [this x] (set! aPrivate x))
  (setBPrivateWithoutDot [this x :- int] (set! bPrivate x))
  (setCPrivateWithoutDot [this x] (set! cPrivate x))
  (setDPrivateWithoutDot [this x] (set! dPrivate x))
  (getAStaticPrivateWithoutDot :- :static [] aStaticPrivate)
  (getBStaticPrivateWithoutDot :- :static [] bStaticPrivate)
  (getCStaticPrivateWithoutDot :- :static [] cStaticPrivate)
  (getDStaticPrivateWithoutDot :- :static [] dStaticPrivate)
  (setAStaticPrivateWithoutDot :- :static [x] (set! aStaticPrivate x))
  (setBStaticPrivateWithoutDot :- :static [x :- int] (set! bStaticPrivate x))
  (setCStaticPrivateWithoutDot :- :static [x] (set! cStaticPrivate x))
  (setDStaticPrivateWithoutDot :- :static [x] (set! dStaticPrivate x))

  ;should fail at runtime, since there's no typehint and reflector
  ;cannot access private fields
  (getFailingOtherAPrivate [this other] (.aPrivate other))
  (getFailingOtherBPrivate [this other] (.bPrivate other))
  (getFailingOtherCPrivate [this other] (.cPrivate other))
  (getFailingOtherDPrivate [this other] (.dPrivate other))

  (getOtherAPrivate [this other :- NonPublicFieldsClass] (.aPrivate other))
  (getOtherBPrivate [this other :- NonPublicFieldsClass] (.bPrivate other))
  (getOtherCPrivate [this other :- NonPublicFieldsClass] (.cPrivate other))
  (getOtherDPrivate [this other :- NonPublicFieldsClass] (.dPrivate other))
  (setOtherAPrivate [this other :- NonPublicFieldsClass x] (set! (.aPrivate other) x))
  (setOtherCPrivate [this other :- NonPublicFieldsClass x] (set! (.cPrivate other) x))
  (setOtherBPrivate [this other :- NonPublicFieldsClass x] (set! (.bPrivate other) x))
  (setOtherDPrivate [this other :- NonPublicFieldsClass x] (set! (.dPrivate other) x))

  (callSampleClass :- :static [x :- SampleClass] (.callFoo x))
  (callSampleClass :- :static [] (.callFoo (SampleClass.))))

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
  :method-declaration-mode :declare

  (ParentClass :- :init [this a] (init-set! a a))
  (foo [this] 0)
  (bar [this] 1)
  (bar [this x] x)
  (protectedMethod :- :protected [this x] x)
  (protectedStaticMethod :- [:protected :static] [x] x))

(defclass ChildClass [c :- :mutable
                      d :- [:mutable :static]]
  :method-declaration-mode :declare

  ParentClass
  (ChildClass :- :init [this a c d]
    (super! a) ;super ctor, required since there's no noarg one
    (init-set! c c))
  (ChildClass :- :static-init [] (set! d "d"))

  (bar [this] (* 2 (super-invoke this bar))) ;call bar from parent
  (bar [this x] (* 2 (super-invoke this bar x)))

  ;get parent field
  (getA [this] (.a this))
  (setA [this a] (set! (.a this) a))
  (getB [this] (ParentClass/b))
  (setB [this b] (set! ParentClass/b b))

  ;not visible fields
  (getCParent [this] (super-get this c))
  (setCParent [this c] (super-set! this c (str c c)))
  (getDParent [this] (ParentClass/d))
  (setDParent [this d] (set! ParentClass/d d))

  ;protected fields from parent
  (getE [this] (.e this))
  (setE [this e] (set! (.e this) e))
  (getF [this] (ParentClass/f))
  (setF [this f] (set! ParentClass/f f))

  ;e won't be visible in this case when using (.e other)
  ;resort to method handles
  (getEOther [this other :- ParentClass] (protected-get other e))
  (setEOther [this other :- ParentClass x] (protected-set! other e x))
  ;but it will be in this one
  (getEOther [this other :- ChildClass] (.e other))
  (setEOther [this other :- ChildClass x] (set! (.e other) x))
  ;handles should also work for current class
  (getEOtherUsingHandle [this other :- ChildClass] (protected-get other e))
  (setEOtherUsingHandle [this other :- ChildClass x] (protected-set! other e x))

  ;calling protected methods and fields from parent
  (protectedParentMethod [this x] (.protectedMethod this x))
  (protectedParentMethod [this other :- ParentClass x] (protected-invoke other protectedMethod x))
  (protectedParentMethod [this other :- ChildClass x] (.protectedMethod other x))
  (protectedParentStaticMethod [this x] (ParentClass/protectedStaticMethod x)))

(defclass ^{Deprecated true} DeprecatedClass
  [^{Deprecated true} a]
  (^{Deprecated true} DeprecatedClass :- :init [this ^{Deprecated true} arg] nil)
  (^{Deprecated true} foo :- :declare [this ^{Deprecated true} arg] nil))

(interface/definterface InferSigInterface
  (foo [^String x]))

(defclass InferMethodSig []
  :method-declaration-mode :infer

  InferSigInterface
  (foo [this x] x) ;; this should be inferref to (foo [^String])
  (foo [this x :- int] x) ;; this should be declared
  (foo :- :declare [this x] x)) ;; this also should be declared