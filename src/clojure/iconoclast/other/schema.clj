; Inspired by the original plumatic/schema

(ns iconoclast.other.schema)

(defn normalized-metadata
  [imeta schema]
  (let [m (reduce (fn [acc x]
                    (if (symbol? x)
                      (assoc acc :tag x)
                      (assoc acc x true)))
                  {} (if (vector? schema) schema (list schema)))]
    (with-meta imeta (merge m (or (meta imeta) {})))))

(defn extract-arrow-schematized-element
  [s]
  (assert (seq s))
  (let [[f & more] s]
    (if (= :- (first more))
      [(normalized-metadata f (second more)) (drop 2 more)]
      [f more])))

(defn process-arrow-schematized-args
  [args]
  (loop [in args out []]
    (if (empty? in)
      out
      (let [[arg more] (extract-arrow-schematized-element in)]
        (recur more (conj out arg))))))

(defn split-spec-with-schema
  [[name & [x & xs]]]
  (if (= :- x)
    `(~[name x (first xs)] ~(second xs) ~@(drop 2 xs))
    `(~[name] ~x ~@xs)))