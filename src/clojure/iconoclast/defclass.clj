(ns iconoclast.defclass
  (:require [iconoclast.other.utils :as utils])
  (:import [clojure.lang IconoclastCompiler]))

(defn- custom-eval [form]
  (IconoclastCompiler/eval form))

(defn- emit-defclass* [tagname name fields interfaces methods]
  (let [nsname (str (namespace-munge *ns*))
        classname (with-meta (symbol (str nsname "." name)) (meta name))]
     (custom-eval
       `(~(symbol "defclass*") ~tagname ~classname ~nsname ~fields
          :implements ~interfaces
          ~@methods))))

(defn- accessor-name [prefix field-name]
  (symbol (str prefix (Character/toUpperCase ^Character (first field-name)) (.substring field-name 1))))

(defn- create-getter [field]
  (let [method-name (with-meta (accessor-name "get" (str field))
                               {:tag (or (:tag (meta field)) 'Object)
                                :defm true})
        this (symbol "this")]
    `(~method-name [~this] (. ~this ~(with-meta field nil)))))

(defn- create-setter [field]
  (let [method-name (with-meta (accessor-name "set" (str field))
                               {:tag 'void :defm true})
        arg (with-meta field {:tag (or (:tag (meta field)) 'Object)})
        this (symbol "this")]
    `(~method-name [~this ~arg] (set! (. ~this ~(with-meta field nil)) ~(with-meta arg nil)))))

(defn- append-getters-setters [fields methods]
  (when (some #(let [m (meta %)]
                  (and (:static m)
                       (or (:get m) (:set m))))
              fields)
    (throw (AssertionError. (str "Getters and setters can be generated only for instance fields"))))
  (reduce (fn [acc x]
            (let [getter (when (:get (meta x)) (create-getter x))
                  setter (when (:set (meta x)) (create-setter x))]
              (cond->> acc
                getter (cons getter)
                setter (cons setter))))
     methods fields))

(defn- ctor-meta [name classname [_ _ & [[head & rest]] :as ctor-form]]
  (if (or (= head 'this!)
          (= head 'super!))
    (apply list (assoc-in (vec ctor-form) [2] (apply list (cons head (map #(utils/update-method-meta name classname %) rest)))))
    ctor-form))

(defn- expand-ctor [name classname fields ctor-form]
  (if (> (count ctor-form) 2)
    (ctor-meta name classname ctor-form)
    (let [[name [this & args]] ctor-form
           name->type (reduce
                        (fn [acc x] (assoc acc x (:tag (meta x))))
                        {} fields)
           args (map
                  (fn [x] (with-meta x (assoc (meta x) :tag (get name->type x))))
                  args)
           body (map (fn [x] `(~(symbol "init-set!") ~(with-meta x nil) ~(with-meta x nil))) args)]
      (cons name (cons (vec (cons this args)) body)))))

(defn- merge-init-with-ctors [name classname fields methods]
  (let [[ctors inits methods] (reduce (fn [acc x]
                                       (let [m (meta (first x))
                                             k (cond
                                                 (:init m) 0
                                                 (:instance-init m) 1
                                                 :else 2)]
                                         (update-in acc [k] #(cons x %))))
                               [[] [] []] methods)
        _ (when (> (count inits) 1) (throw (AssertionError. "Only one instance init section is allowed")))
        ctors (map (partial expand-ctor name classname fields) ctors)
        init (first inits)]
    (if (empty? init)
      (reduce #(cons %2 %1) methods ctors)
      (let [init-this (-> init second first)
            init-logic (drop 2 init)
            blank-ctor (list (with-meta name {:init true}) [init-this] nil)
            ctors (map (fn [x]
                          (let [[_ [ctor-this & _] super & _] x
                                idx (if (some #(= (first super) %) ['super! 'this!]) 3 2)
                                [head tail] (split-at idx x)]
                            (when (and (not= ctor-this init-this)
                                       (not= init-this (symbol "_")))
                              (throw (AssertionError. (str "'this' name in ctor and instance init section have to "
                                                       "be equal. Got: " init-this " and " ctor-this))))
                            (apply concat [head init-logic tail])))
                    (if-not (empty? ctors) ctors (cons blank-ctor ctors)))]
        (reduce #(cons %2 %1) methods ctors)))))

(defmacro defclass [& class-spec]
  "See deftype documentation.

  Name supports additional metadata:
  :nonfinal - makes class non final (by default class is final)
  :abstract - makes class abstract
  :load-ns - makes class automatically load enclosing ns

  Fields:

  Typehinted fields will have the type of the hint. By default
  fields are final (immutable).

  Additional metadata on field name:
  :private, :public, :protected - visibility modifiers
  :static - makes field static
  :mutable - same as :unsychronized-mutable
  :array - field will be an array, in order to specify number of
           dimensions use {:array n}. If no typehint is specified
           it will be an array of Objects, otherwise an array of
           type from the typehint
  :get, :set - will generate public getter and setter for the field
               Method names are (for field F) getF and setF.

  Methods:

  Methods not in interfaces can be declared using :defm metadata
  on methodname. Hint on methodname will define return type and
  hints on methodargs will define the types of the arguments.

  Methods can be marked as constructors using :init metadata on
  methodname. To call a construtor from parent you can use
  (super! args*) as the first element of the constructor body.
  To call other constructor from the same class use (this! args).
  If no constructor is declared one constructor setting all fields
  will be generated (like in deftype). You can also declare
  constructors on the form of:
  (^:init name [this field*]) - if all fields names can be matched
  with fields from fields declaration and there is no body logic
  will be generated automatically. Such constructor will set fields
  that have the names that are equal to name of its arguments.

  You can define initialization blocks per instance and per class.
  To define instance initializitaion block mark method name with
  :instance-init metadata. Instance init accepts only one argument
  that will correspond to 'this'. It will be merged with constructors
  and called before their logic.

  To create a static initialization block mark method name with
  :static-init.

  In order to initialize final fields in constructors or initialization
  blocks use (init-set! field valexpr). Field has to be a symbol, as
  defined in fields.

  Additional metadata on method name:
  :private, :public, :protected - visibility modifiers
  :final - makes method final
  :static - makes method static. Static methods don't have the first
            parameter, that corresponds to the target object ('this')
  :abstract - makes method abstract
  :defm - marks method as declared in this class
  :init - marks method as constructor
  :instance-init - marks method as instance init block
  :static-init - marks method as static init block
  :varargs - marks method as a vararg method, last argument of the method
             has to be an array
  :array - same as for fields, makes return type an array

  Additional metadata on method arguments:
  :array - same as for fields

  Parent class:
  Add parent class just like interface in spec. There can be only
  one inherited class.

  Accessing fields and methods from parent class:
  If the field is not visible and would be accessed using .super
  in Java it can be accessed using iconoclast.reflect/s-get and s-set!
  macros: (s-get instance field) and (s-set! instance field value).
  Methods from parent class that are also defined in inheriting class
  can be accessed using iconoclast.reflect/s-invoke:
  (s-invoke instance method & args)


  Accessing protected fields and method from parent class:
  When accessing protected fields or methods from instance
  of a parent class you can use iconoclast.reflect/p-get, p-set!
  and p-invoke.

  No factory functions will be defined."
  (let [[name fields & opts+specs] (utils/merge-schema-with-meta class-spec)
        _ (utils/validate-fields fields name)
        gname name
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." gname))
        [interfaces methods opts] (utils/parse-opts+specs opts+specs name classname)
        _ (utils/validate-options opts)
        hinted-fields (utils/update-fields-meta name classname fields opts)
        methods (->> methods
                     (merge-init-with-ctors name classname hinted-fields)
                     (append-getters-setters hinted-fields))]
    `(do
       ~(emit-defclass* name gname (vec hinted-fields) (vec interfaces) methods)
        (import ~classname)
        ~classname)))