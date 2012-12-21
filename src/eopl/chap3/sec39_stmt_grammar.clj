(ns eopl.chap3.sec39-stmt-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grammar
;; program
(defrecord AProgram [stmt])

;; statement
(defrecord AssignStatement [id exp])
(defrecord PrintStatement [exp])
(defrecord CompoundStatement [stmts])
(defrecord IfStatement [exp true-stmt false-stmt])
(defrecord WhileStatement [exp stmt])
(defrecord BlockStatement [ids body])

;; expression
(defrecord LitExp [datum])
(defrecord VarExp [id])
(defrecord PrimappExp [prim rands])
(defrecord IfExp [test-exp true-exp false-exp])
(defrecord LetExp [ids rands body])
(defrecord ProcExp [ids body])
(defrecord AppExp [rator rands])
(defrecord LetrecExp [proc-names idss bodies letrec-body])
(defrecord VarassignExp [id rhs-exp])
(defrecord BeginExp [exp exps])

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
