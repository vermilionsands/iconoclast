(ns hydra.defclass
  (:use [hydra.defclass.utils])
  (:import [clojure.lang HydraCompiler]))

(defn- custom-eval [form]
  (HydraCompiler/eval form))

(defn- emit-defclass* [tagname name fields interfaces methods]
  (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))]
     (custom-eval
       `(~(symbol "defclass*") ~tagname ~classname ~fields
          :implements ~interfaces
          ~@methods))))

(defmacro defclass [name fields & opts+specs]
  (validate-fields fields)
  (let [gname name
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." gname))
        [interfaces methods opts] (parse-opts+specs opts+specs name classname)
        hinted-fields (tweak-fields-meta fields name classname)
        fields (vec (map #(with-meta % nil) fields))]
        ;[field-args over] (split-at 20 fields)]
    `(let []
       ~(emit-defclass* name gname (vec hinted-fields) (vec interfaces) methods)
       (import ~classname)
       ;no positional factory and other such things for now
       ;~(build-positional-factory gname classname fields)
       ~classname)))
