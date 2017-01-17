(ns iconoclast.reflect
  (:require [iconoclast.other.utils :refer [resolve-tag]])
  (:import [java.lang NoSuchMethodException]
           [java.lang.invoke MethodHandles MethodType]
           [java.lang.reflect Field Method]
           [java.util List]))

(defn get-class [instance use-parent?]
  (if use-parent?
    (first (bases (class instance)))
    (class instance)))

(defn get-handle-target-and-field
  ([clazz field]
   (get-handle-target-and-field clazz field clazz))
  ([clazz field inital-class]
   (let [field-type (try
                      (.getType ^Field (.getDeclaredField ^Class clazz field))
                      (catch Exception _ nil))]
     (if field-type
       [clazz field-type]
       (let [parent (first (bases clazz))]
         (if parent
           (recur (first (bases clazz)) field inital-class)
           (throw (IllegalArgumentException.
                    (str "Cannot find field " field " in " inital-class " or it's ancestors")))))))))

(defmacro protected-invoke [instance m & args]
  (let [tags (vec (map #(:tag (:meta %)) args))]
    `(let [clazz# (class ~instance)
           arg-types# (when (not-empty ~tags) (into-array Class (vec (map resolve-tag ~tags))))
           method# (try (.getDeclaredMethod ^Class clazz# ~(str m) arg-types#)
                        (catch NoSuchMethodException e#
                          (throw (IllegalArgumentException.
                                   (str "Cannot find method " ~(str m) " for class " clazz#
                                     " with signature " (vec arg-types#) ". Consider using typehints")))))]
           ;return-type# (.getReturnType ^Method method#)]
       (if (empty? arg-types#)
         (.invoke ^Method method# ~instance)
         (.invoke ^Method method# ~instance (into-array Object (list ~@args)))))))

(defmacro super-invoke [instance m & args]
  (let [tags (vec (map #(:tag (:meta %)) args))]
    `(let [clazz# (class ~instance)
           target# (get-class ~instance true)
           arg-types# (when (not-empty ~tags) (into-array Class (vec (map resolve-tag ~tags))))
           method# (try (.getDeclaredMethod ^Class target# ~(str m) arg-types#)
                        (catch NoSuchMethodException e#
                          (throw (IllegalArgumentException.
                                   (str "Cannot find method " ~(str m) " for class " target#
                                     " with signature " (vec arg-types#) ". Consider using typehints")))))
           return-type# (.getReturnType ^Method method#)
           handle# (.findSpecial (MethodHandles/lookup)
                     target#
                      ~(str m)
                      (if (empty? arg-types#)
                        (MethodType/methodType ^Class return-type#)
                        (MethodType/methodType ^Class return-type# ^{:tag "[Ljava.lang.Class;"} arg-types#))
                      clazz#)]
       (.invokeWithArguments handle# ^List (list ~instance ~@args)))))

(defmacro invoke-field-handle [use-parent? instance field & arg]
  (when (> (count arg) 1)
    (throw (AssertionError. "Supports only one optional argument")))
  `(let [clazz# (get-class ~instance ~use-parent?)
         [target# field-type#] (get-handle-target-and-field clazz# ~(str field))
         handle# (. (MethodHandles/lookup) ~(if (empty? arg) 'findGetter 'findSetter)
                    target# ~(str field) field-type#)]
     (.invokeWithArguments handle# ^List (list ~instance ~@arg))))

(defmacro protected-get [instance f]
  `(invoke-field-handle false ~instance ~f))

(defmacro protected-set! [instance f arg]
  `(invoke-field-handle false ~instance ~f ~arg))

(defmacro super-get [this f]
  `(invoke-field-handle true ~this ~f))

(defmacro super-set! [this f arg]
  `(invoke-field-handle true ~this ~f ~arg))
