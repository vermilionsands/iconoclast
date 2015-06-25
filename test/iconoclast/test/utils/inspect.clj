(ns iconoclast.test.utils.inspect
  (:require [clojure.test :refer :all]))

(defn sig [& c] (into-array Class c))

(defn array
  ([c] (array c 1))
  ([c d] (.getClass (apply make-array c (repeat d 0)))))

(defn get-class-modifier [c m]
  (bit-and m (.getModifiers c)))

(defn get-method-modifier [c m-name m-sig m]
  (bit-and m (.getModifiers (.getDeclaredMethod c m-name m-sig))))

(defn get-method-return-class [c m-name m-sig]
  (.getReturnType (.getDeclaredMethod c m-name m-sig)))

(defn method-exists? [c m-name m-sig]
  (some?
    (try
      (.getDeclaredMethod c m-name m-sig)
      (catch Exception _ false))))

(defn ctor-exists? [c c-sig]
  (some? (try
           (.getDeclaredConstructor c c-sig)
           (catch Exception _ nil))))

(defn get-field-modifier [c f m]
  (bit-and m (.getModifiers (.getDeclaredField c f))))

(defn get-field-class [c f-name]
  (.getType (.getDeclaredField c f-name)))
