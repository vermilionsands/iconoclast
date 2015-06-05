;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Slightly modified utilities from core_deftype.clj, from original clojure distribution.

(ns hydra.defclass.utils)

(defn get-array-type
  ([type]
    (get-array-type type 1))
  ([type d]
    (when (< d 1)
      (throw (IllegalArgumentException. "Array dimension cannot be lower than 1")))
    (.getClass (apply make-array type (repeat d 0)))))

(defn symbol-to-classname [sym]
  "Resolve symbol to classname"
  (let [name (str sym)
        clazz (try
                (if (> (.indexOf name ".") 0)
                    (clojure.lang.RT/classForName name)
                    (let [c (.getMapping *ns* sym)]
                   (if (class? c)
                     c
                     (clojure.lang.RT/classForName name))))
               (catch Exception _ nil))]
    (when (nil? clazz)
      (throw (IllegalArgumentException. (str "Cannot resolve class for " name))))
    clazz))

(defn- meta-arr-to-hint [classname m]
  "Gets the type hint for arrays declared as ^:arr ^typehint"
  (let [d (:array m)
        t (:tag m)]
    (if-not (or (nil? d) (nil? t))
      (assoc m :tag
        (if (= t classname)
          (symbol (str (apply str (repeat d "[")) "L" t ";"))
          (.getName (get-array-type (symbol-to-classname t) d))))
      m)))

(defn- meta-mutable-to-unsynchronized [m]
  (if (:mutable m)
    (-> m (dissoc :mutable) (assoc :unsynchronized-mutable true))
    m))

(defn- meta-self-hint [name classname m]
  (let [t (:tag m)]
    (if (and (not (nil? t)) (= name t))
      (assoc m :tag classname)
      m)))

;TODO add tests
(defn tweak-fields-meta
  "Tweaks metadata replacing :mutable with :unsychronized-mutable"
  [fields classname ns+classname]
  (let [meta-self-hint (partial meta-self-hint classname ns+classname)
        meta-arr-to-hint (partial meta-arr-to-hint ns+classname)]
    (vec
      (map (fn [x]
             (with-meta x (-> (meta x) meta-mutable-to-unsynchronized
                                       meta-self-hint
                                       meta-arr-to-hint)))
        fields))))

(defn tweak-method-meta
  [sym classname ns+classname]
  (let [meta-self-hint (partial meta-self-hint classname ns+classname)
        meta-arr-to-hint (partial meta-arr-to-hint ns+classname)]
    (with-meta sym (-> (meta sym) meta-self-hint
                                  meta-arr-to-hint))))

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

(defn validate-fields [fields]
  (when-not (vector? fields)
    (throw (AssertionError. "No fields vector given.")))
  (let [specials #{'__meta '__extmap}]
    (when (some specials fields)
      (throw (AssertionError. (str "The names in " specials " cannot be used as field names for types or records."))))))

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

(defn parse-opts+specs [opts+specs classname ns+classname]
  (let [[opts specs] (parse-opts opts+specs)
        impls (parse-impls specs)
        interfaces (-> (map #(if (var? (resolve %))
                               (:on (deref (resolve %)))
                               %)
                            (keys impls))
                       set
                       (disj 'Object 'java.lang.Object 'not-in-interface)
                       vec)
        methods (map (fn [[name params & body]]
                       (cons name (maybe-destructured params body)))
                     (apply concat (vals impls)))
        methods (let [f (fn [x] (tweak-method-meta x classname ns+classname))]
                  (map (fn [[name params & body]]
                         (let [name (f name)
                               params (vec (map f params))]
                           (reduce #(cons %2 %1) body [params name])))
                       methods))]
    (when-let [bad-opts (seq (remove #{:no-print} (keys opts)))]
      (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
    [interfaces methods opts]))

;TODO add test
(defn has-self-hints?
  "Checks if class has hints to itself"
  [classname name fields methods]
  (let [v (flatten (concat [name] [fields] (map (fn [[name params _]] [name params]) methods)))]
    (some #(= (:tag (meta %)) classname) v)))
