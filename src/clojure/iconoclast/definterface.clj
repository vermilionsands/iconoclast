(ns iconoclast.definterface
  (:refer-clojure :exclude [definterface])
  (:use [iconoclast.other.utils]))

(defn- get-interface-tag
  [classname ns+classname sym]
  (let [default-tag (fn [x] (if (:tag x) x (assoc x :tag 'Object)))]
    (:tag (->> (meta sym) default-tag
                          (meta-self-hint classname ns+classname)
                          (meta-arr-to-hint ns+classname)))))

(defmacro definterface [name & sigs]
  (let [cname (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))
        tag (partial get-interface-tag name cname)
        psig (fn [[name [& args]]]
               (vector name (vec (map tag args)) (tag name) (map meta args)))]
    `(let []
       (gen-interface :name ~cname :methods ~(vec (map psig sigs)))
       (import ~cname))))
