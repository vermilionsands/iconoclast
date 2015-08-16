(ns iconoclast.defclass
  (:use [iconoclast.other.utils])
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
  (let [[ctors inits methods] (reduce (fn [acc x]
                                       (let [m (meta (first x))
                                             k (cond
                                                 (:init m) 0
                                                 (:instance-init m) 1
                                                 :else 2)]
                                         (update-in acc [k] #(cons x %))))
                               [[] [] []] methods)
        _ (when (> (count inits) 1) (throw (AssertionError. "Only one instance init block is allowe")))
        ctors (map (partial expand-ctor name classname fields) ctors)
        init (first inits)]
    (if (empty? init)
      (reduce #(cons %2 %1) methods ctors)
      (let [init-this (-> init second first)
            init-logic (drop 2 init)
            blank-ctor (list (with-meta name {:init true}) [init-this] nil)
            ctors (map (fn [x]
                          (let [[_ [ctor-this & args] super & body] x
                                idx (if (some #(= (first super) %) ['super! 'this!]) 3 2)
                                [head tail] (split-at idx x)]
                            (when (and (not= ctor-this init-this)
                                       (not= init-this (symbol "_")))
                              (throw (AssertionError. (str "'this' name in ctor and instance init section have to "
                                "be equal. Got: " init-this " and " ctor-this))))
                            (apply concat [head init-logic tail])))
                    (if-not (empty? ctors) ctors (cons blank-ctor ctors)))]
        (reduce #(cons %2 %1) methods ctors)))))

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
