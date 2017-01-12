(ns iconoclast.reflect
  (:require [iconoclast.other.utils :refer [resolve-tag]]))

(defmacro get-class-from-instance [use-parent? instance]
  `[(class ~instance)
    ~(if use-parent? `(first (bases (class ~instance)))
                     `(class ~instance))])

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
