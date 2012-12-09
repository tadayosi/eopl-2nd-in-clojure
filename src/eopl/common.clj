(ns eopl.common)

(defmacro define-datatype
  [record factory args]
  `(do
     (defrecord ~record ~args)
     (defn ~factory ~args (new ~record ~@args))))
