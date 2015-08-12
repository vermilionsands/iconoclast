(ns iconoclast.defclass
  (:use [iconoclast.other.utils])
  (:import [clojure.lang IconoclastCompiler]))

(defn- custom-eval [form]
  (IconoclastCompiler/eval form))

(defn- emit-defclass* [tagname name fields interfaces methods]
  (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))]
     (custom-eval
       `(~(symbol "defclass*") ~tagname ~classname ~fields
          :implements ~interfaces
          ~@methods))))

(defn- accessor-name [prefix field-name]
  (symbol (str prefix (Character/toUpperCase (first field-name)) (.substring field-name 1))))

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
    (apply list (assoc-in (vec ctor-form) [2] (apply list (cons head (map #(update-method-meta name classname %) rest)))))
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
  (let [[ctors init methods] (reduce (fn [acc x]
                                       (let [m (meta (first x))
                                             k (cond
                                                 (:init m) 0
                                                 (:instance-init m) 1
                                                 :else 2)]
                                         (update-in acc [k] #(cons x %))))
                               [[] [] []] methods)
        ctors (map (partial expand-ctor name classname fields) ctors)]
        (if (empty? init)
          (reduce #(cons %2 %1) methods ctors)
          (let [init-this (-> init first second first)
                init-logic (drop 2 (first init))
                blank-ctor (list (with-meta name {:init true}) [init-this] nil)
                ctors (map (fn [x]
                              (let [[_ [ctor-this & args] super & body] x
                                    idx (if (some #(= (first super) %) ['super! 'this!]) 3 2)
                                    [head tail] (split-at idx x)]
                                (when (not= ctor-this init-this)
                                  (throw (AssertionError. (str "'this' name in ctor and instance init section have to "
                                    "be equal. Got: " init-this " and " ctor-this))))
                                (apply concat [head init-logic tail])))
                        (if-not (empty? ctors) ctors (cons blank-ctor ctors)))]
            (reduce #(cons %2 %1) methods ctors)))))

(defmacro get-class-from-instance [use-parent? instance]
  `[(class ~instance)
    ~(if use-parent? `(first (bases (class ~instance)))
                     `(class ~instance))])

(defmacro p-invoke [instance m & args]
  `(invoke-method true ~instance ~m ~@args))

(defmacro s-invoke' [instance m & args]
  `(invoke-method false ~instance ~m ~@args))

(defmacro p-invoke [instance m & args]
  (let [tags (vec (map #(:tag (:meta %)) args))]
    `(let [clazz# (class ~instance)
           arg-types# (when (not-empty ~tags) (into-array Class (vec (map resolve-tag ~tags))))
           method# (try (.getDeclaredMethod ^Class clazz# ~(str m) arg-types#)
                        (catch java.lang.NoSuchMethodException e#
                          (throw (IllegalArgumentException.
                                   (str "Cannot find method " ~(str m) " for class " clazz#
                                     " with signature " (vec arg-types#) ". Consider using typehints")))))
           return-type# (.getReturnType ^java.lang.reflect.Method method#)]
        (if (empty? arg-types#)
            (.invoke ^java.lang.reflect.Method method# ~instance)
            (.invoke ^java.lang.reflect.Method method# ~instance (into-array Object (list ~@args)))))))

(defmacro s-invoke [instance m & args]
  (let [tags (vec (map #(:tag (:meta %)) args))]
    `(let [[clazz# parent#] (get-class-from-instance true ~instance)
           arg-types# (when (not-empty ~tags) (into-array Class (vec (map resolve-tag ~tags))))
           method# (try (.getDeclaredMethod ^Class parent# ~(str m) arg-types#)
                        (catch java.lang.NoSuchMethodException e#
                          (throw (IllegalArgumentException.
                                   (str "Cannot find method " ~(str m) " for class " parent#
                                     " with signature " (vec arg-types#) ". Consider using typehints")))))
           return-type# (.getReturnType ^java.lang.reflect.Method method#)
           handle# (.findSpecial (java.lang.invoke.MethodHandles/lookup)
                      parent#
                      ~(str m)
                      (if (empty? arg-types#)
                        (java.lang.invoke.MethodType/methodType ^Class return-type#)
                        (java.lang.invoke.MethodType/methodType ^Class return-type# ^{:tag "[Ljava.lang.Class;"} arg-types#))
                      clazz#)]
      (.invokeWithArguments handle# ^java.util.List (list ~instance ~@args)))))

(defmacro invoke-field-handle [use-parent? instance field & arg]
  (when (> (count arg) 1)
    (throw (AssertionError. "Supports only one optional argument")))
  `(let [[clazz# parent#] (get-class-from-instance ~use-parent? ~instance)
         field-type# (.getType ^java.lang.reflect.Field (.getDeclaredField ^Class parent# ~(str field)))
         handle# (. (java.lang.invoke.MethodHandles/lookup) ~(if (empty? arg) 'findGetter 'findSetter)
                   parent# ~(str field) field-type#)]
     (.invokeWithArguments handle# ^java.util.List (list ~instance ~@arg))))

(defmacro p-get [instance f]
  `(invoke-field-handle false ~instance ~f))

(defmacro p-set! [instance f arg]
  `(invoke-field-handle false ~instance ~f ~arg))

(defmacro s-get [this f]
  `(invoke-field-handle true ~this ~f))

(defmacro s-set! [this f arg]
  `(invoke-field-handle true ~this ~f ~arg))

(defmacro defclass [name fields & opts+specs]
  (validate-fields fields name)
  (let [gname name
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." gname))
        [interfaces methods opts] (parse-opts+specs opts+specs name classname)
        hinted-fields (update-fields-meta name classname fields)
        methods (->> methods
                     (merge-init-with-ctors name classname hinted-fields)
                     (append-getters-setters hinted-fields))]
    `(do
       ~(emit-defclass* name gname (vec hinted-fields) (vec interfaces) methods)
       (import ~classname)
       ~classname)))
