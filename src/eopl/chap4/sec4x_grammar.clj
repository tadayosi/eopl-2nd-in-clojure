(ns eopl.chap4.sec4x-grammar
  (:use clojure.pprint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grammar
;; program
(defrecord AProgram [exp])

;; expression
(defrecord LitExp [datum])
(defrecord VarExp [id])
(defrecord PrimappExp [prim rands])
(defrecord IfExp [test-exp true-exp false-exp])
(defrecord LetExp [ids rands body])
(defrecord ProcExp [arg-texps ids body])
(defrecord AppExp [rator rands])
(defrecord LetrecExp [result-texps proc-names
                      args-texpss idss bodies
                      letrec-body])
(defrecord VarassignExp [id rhs-exp])
(defrecord BeginExp [exp exps])
(defrecord TrueExp [])
(defrecord FalseExp [])
(defrecord LettypeExp [type-name texp result-texps proc-names
                       arg-texpss idss bodies
                       lettype-body])

;; type-exp
(defrecord IntTypeExp [])
(defrecord BoolTypeExp [])
(defrecord ProcTypeExp [arg-texps result-texp])
(defrecord TidTypeExp [id])

;; primitive
(defrecord AddPrim [])
(defrecord SubtractPrim [])
(defrecord MultPrim [])
(defrecord IncrPrim [])
(defrecord DecrPrim [])
(defrecord ZeroTestPrim [])

;; procval
(defrecord Closure [ids body env])

;; reference
(defrecord ARef [position vec])

;; type
(defrecord AtomicType [name])
(defrecord ProcType [arg-types result-type])
(def int-type (AtomicType. 'int))
(def bool-type (AtomicType. 'bool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on data types
(defn true-value? [x]
  (not (zero? x)))

;; reference
(defn primitive-deref [ref]
  (condp instance? ref
    ARef (@(:vec ref) (:position ref))))
(defn primitive-setref! [ref val]
  (condp instance? ref
    ARef (swap! (:vec ref) assoc (:position ref) val)))
(defn deref_ [ref]
  (primitive-deref ref))
(defn setref! [ref val]
  (primitive-setref! ref val))

;; type
(declare expand-type-expressions)
(defn expand-type-expression [texp tenv]
  (condp instance? texp
    TidTypeExp ((resolve 'eopl.chap4.sec4x-tenv/find-typedef)
                 tenv (:id texp))
    IntTypeExp int-type
    BoolTypeExp bool-type
    ProcTypeExp (ProcType.
                  (expand-type-expressions (:arg-texps texp) tenv)
                  (expand-type-expression (:result-texp texp) tenv))
    (throw (Exception.
             (str 'expand-type-expression
                  ": Invalid type expression " texp)))))
(defn expand-type-expressions [texps tenv]
  (map
    (fn [texp]
      (expand-type-expression texp tenv))
    texps))
