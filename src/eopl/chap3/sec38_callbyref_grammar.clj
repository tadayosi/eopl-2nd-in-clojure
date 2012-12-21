(ns eopl.chap3.sec38-callbyref-grammar)

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
;; target
(defrecord DirectTarget [expval])
(defrecord IndirectTarget [ref])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on data types
(defn true-value? [x]
  (not (zero? x)))

;; target
(defn expval? [x]
  (or (number? x) (instance? Closure x)))
(defn ref-to-direct-target? [x]
  (and
    (instance? ARef x)
    (condp instance? x
      ARef (condp instance? (@(:vec x) (:position x))
             DirectTarget true
             IndirectTarget false))))
;; reference
(defn primitive-deref [ref]
  (condp instance? ref
    ARef (@(:vec ref) (:position ref))))
(defn primitive-setref! [ref val]
  (condp instance? ref
    ARef (swap! (:vec ref) assoc (:position ref) val)))
(defn deref_ [ref]
  (let [target (primitive-deref ref)]
    (condp instance? target
      DirectTarget (:expval target)
      IndirectTarget (let [target1 (primitive-deref (:ref target))]
                       (condp instance? target1
                         DirectTarget (:expval target1)
                         IndirectTarget (throw (Exception.
                                                 (str 'deref
                                                      ": Illegal reference: " (:ref target)))))))))
(defn setref! [ref expval]
  (let [ref (let [target (primitive-deref ref)]
              (condp instance? target
                DirectTarget ref
                IndirectTarget (:ref target)))]
    (primitive-setref! ref (DirectTarget. expval))))
