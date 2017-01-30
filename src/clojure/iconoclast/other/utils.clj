;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Based on the core_deftype.clj from the original clojure distribution.
;   vermilionsands 2015

(ns iconoclast.other.utils
  (:require [clojure.string :as string]
            [iconoclast.other.schema :as schema]))

(def hardcoded-classes
  (let [f (fn [c] (.getClass (make-array c 0)))]
    {'objects  (f Object)
     'ints     (f (Integer/TYPE))
     'longs    (f (Long/TYPE))
     'floats   (f (Float/TYPE))
     'doubles  (f (Double/TYPE))
     'chars    (f (Character/TYPE))
     'shorts   (f (Short/TYPE))
     'bytes    (f (Byte/TYPE))
     'booleans (f (Boolean/TYPE))
     'int      (Integer/TYPE)
     'long     (Long/TYPE)
     'float    (Float/TYPE)
     'double   (Double/TYPE)
     'char     (Character/TYPE)
     'short    (Short/TYPE)
     'byte     (Byte/TYPE)
     'boolean  (Boolean/TYPE)
     'void     (Void/TYPE)}))

(def supported-options #{:no-print :load-ns :fields-mutability :fields-visibility :method-declaration-mode})

(def field-mutability-keys #{:mutable :unsynchronized-mutable :volatile-mutable :final})

(def field-visibility-keys #{:private :protected :public})

(def method-declaration-mode-keys #{:override :declare :infer})

(defn- resolve-classname [sym]
  (let [name (str sym)]
    (try
      (if (> (.indexOf name ".") 0)
          (clojure.lang.RT/classForName name)
          (let [c (.getMapping ^clojure.lang.Namespace *ns* sym)]
            (if (class? c)
              c
              (clojure.lang.RT/classForName name))))
      (catch Exception _ nil))))

(defn resolve-tag [tag]
  (if (class? tag)
    tag
    (let [sym (if (or (nil? tag) (symbol? tag)) (or tag 'Object) (symbol tag))
          clazz (or (hardcoded-classes sym)
                    (resolve-classname sym))]
      (when (nil? clazz)
        (throw (IllegalArgumentException. (str "Cannot resolve class for '" sym "'"))))
      clazz)))

(defn get-array-type
  ([type] (get-array-type type 1))
  ([type d]
   (when (< d 1)
     (throw (IllegalArgumentException. "Array dimension cannot be lower than 1")))
   (.getClass (apply make-array type (repeat d 0)))))

(defn meta-arr-to-hint [classname m]
  (let [d (:array m)
        d (if (number? d)
            d
            (if (true? d) 1 nil))
        t (or (:tag m) 'Object)]
    (if-not (or (nil? d))
      (assoc m :tag
        (symbol
          (if (= t classname)
            (str (apply str (repeat d "[")) "L" t ";")
            (.getName (get-array-type (resolve-tag t) d)))))
      m)))

(defn meta-mutable-to-unsynchronized [m]
  (if (:mutable m)
    (-> m (dissoc :mutable) (assoc :unsynchronized-mutable true))
    m))

(defn meta-self-hint [classname m]
  (let [name (symbol (last (string/split (name classname) #"\.")))
        t (:tag m)]
    (if (and (not (nil? t)) (= name t))
      (assoc m :tag classname)
      m)))

(defn meta-add-default-option [option-set k m]
  (if (and (seq (select-keys m option-set)) k)
    m
    (assoc m k true)))

(defn update-fields-meta [classname fields {:keys [fields-mutability fields-visibility]}]
  (mapv (fn [x]
          (with-meta x (->> (meta x)
                            (meta-add-default-option field-mutability-keys fields-mutability)
                            (meta-add-default-option field-visibility-keys fields-visibility)
                            (meta-mutable-to-unsynchronized)
                            (meta-self-hint classname)
                            (meta-arr-to-hint classname))))
    fields))

(defn update-method-meta [classname {:keys [method-declaration-mode]} sym]
  (with-meta sym (->> (meta sym)
                      (meta-add-default-option method-declaration-mode-keys method-declaration-mode)
                      (meta-self-hint classname)
                      (meta-arr-to-hint classname))))

(defn- maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params []
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

(defn validate-fields [fields name]
  (when-not (vector? fields)
    (throw (AssertionError. "No fields vector given.")))
  (let [specials #{'__meta '__extmap}]
    (when (some specials fields)
      (throw (AssertionError. (str "The names in " specials " cannot be used as field names for types or records.")))))
  (let [non-syms (remove symbol? fields)]
    (when (seq non-syms)
      (throw (clojure.lang.Compiler$CompilerException.
              *file*
              (.deref clojure.lang.IconoclastCompiler/LINE)
              (.deref clojure.lang.IconoclastCompiler/COLUMN)
              (AssertionError.
               (str "defclass fields must be symbols, "
                    *ns* "." name " had: "
                    (apply str (interpose ", " non-syms)))))))))

(defn validate-options [opts]
  (when-let [bad-opts (seq (remove supported-options (keys opts)))]
    (throw (IllegalArgumentException. ^String (apply print-str "Unsupported option(s) -" bad-opts)))))

(defn- parse-opts [s]
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defn- parse-impls [specs]
  (loop [ret {} s (if-not (symbol? (first specs)) (cons 'not-in-interface specs) specs)]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(defn parse-opts+specs [opts+specs classname]
  (let [[opts specs] (parse-opts opts+specs)
        impls (parse-impls specs)
        interfaces (-> (map #(if (var? (resolve %))
                               (:on (deref (resolve %)))
                               %)
                            (keys impls))
                       set
                       (disj 'Object 'java.lang.Object 'not-in-interface)
                       vec)
        methods (let [f (fn [x] (update-method-meta classname opts x))]
                  (->>
                    (apply concat (vals impls))
                    (map (fn [[name params & body]]
                           (cons name (maybe-destructured params body))))
                    (map (fn [[name params & body]]
                           (reduce #(cons %2 %1) body [(vec (map f params)) (f name)])))))]
    [interfaces methods opts]))

(defn merge-schema-with-meta
  ([spec]
   (merge-schema-with-meta spec true))
  ([spec recur?]
   (let [[name+spec fields+spec & more] (schema/split-spec-with-schema spec)
         [name] (schema/process-arrow-schematized-args name+spec)
         fields (schema/process-arrow-schematized-args fields+spec)
         more (if-not (and more recur?)
                more
                (loop [out [] [x y & rs :as xs] more]
                  (cond
                    (empty? xs)  out
                    ;; interfaces and classes
                    (symbol? x)  (recur (conj out x) (rest xs))
                    ;; options
                    (keyword? x) (recur (conj out x y) rs)
                    ;; methods
                    :else        (recur (conj out `(~@(merge-schema-with-meta x false))) (rest xs)))))]
     `(~name ~fields ~@more))))