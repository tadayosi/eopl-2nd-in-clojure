(ns eopl.chap4.sec44-typeinf-grammar
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

;; optional-type-exp
(defrecord ATypeExp [texp])
(defrecord NoTypeExp [])

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
(defrecord TvarType [serial-number container])
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
(declare expand-optional-type-expressions expand-optional-type-expression)
(defn expand-type-expression [texp tenv]
  (condp instance? texp
    TidTypeExp ((resolve 'eopl.chap4.sec44-typeinf-tenv/find-typedef)
                 tenv (:id texp))
    IntTypeExp int-type
    BoolTypeExp bool-type
    ProcTypeExp (ProcType.
                  (expand-optional-type-expressions (:arg-texps texp) tenv)
                  (expand-optional-type-expression (:result-texp texp) tenv))
    (throw (Exception.
             (str 'expand-type-expression
                  ": Invalid type expression " texp)))))
(defn expand-optional-type-expressions [texps tenv]
  (map
    (fn [texp]
      (expand-optional-type-expression texp tenv))
    texps))

(declare fresh-tvar tvar-type->container)
(defn expand-optional-type-expression [otexp tenv]
  (condp instance? otexp
    NoTypeExp (fresh-tvar)
    ATypeExp (expand-type-expression (:texp otexp) tenv)))
(def fresh-tvar
  (let [serial-number (atom 0)]
    (fn []
      (swap! serial-number + 1)
      (TvarType. @serial-number (atom [])))))
(defn tvar->contents [ty]
  (@(tvar-type->container ty) 0))
(defn tvar-set-contents! [ty val]
  (swap! (tvar-type->container ty) assoc 0 val))
(defn tvar-non-empty? [ty]
  (not (empty? @(tvar-type->container ty))))
(defn tvar-type->container [ty]
  (condp instance? ty
    TvarType (:container ty)
    (throw (Exception.
             (str 'tvar-type->container
                  ": Not a tvar-type: " ty)))))
