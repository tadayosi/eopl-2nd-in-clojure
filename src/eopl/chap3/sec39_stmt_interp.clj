(ns eopl.chap3.sec39-stmt-interp
  (:use eopl.common))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment
(define-datatype EmptyEnvRecord empty-env-record [])
(define-datatype ExtendedEnvRecord extended-env-record [syms vals env])

(declare apply-env apply-env-ref list-find-position list-index rib-find-position)
(declare closure a-ref deref_ setref!)
(defn empty-env []
  (empty-env-record))
(defn extend-env [syms vals env]
  (extended-env-record syms (atom (vec vals)) env))
(defn extend-env-recursively [proc-names idss bodies old-env]
  (let [len (count proc-names)
        vec (atom [])
        env (extended-env-record
              proc-names vec old-env)]
    (do
      (doseq [pos (range len)]
        (swap! vec conj (closure
                          (nth idss pos)
                          (nth bodies pos)
                          env)))
      env)))
(defn apply-env [env sym]
  (deref_ (apply-env-ref env sym)))
(defn apply-env-ref [env sym]
  (condp instance? env
    EmptyEnvRecord (throw (Exception. (str 'apply-env-ref ": No binding for " sym)))
    ExtendedEnvRecord (let [pos (rib-find-position sym (:syms env))]
                        (if (number? pos)
                          (a-ref pos (:vals env))
                          (apply-env-ref (:env env) sym)))))
(defn list-find-position [sym los]
  (list-index (fn [sym1] (= sym1 sym)) los))
(defn list-index [pred ls]
  (cond
    (empty? ls) false
    (pred (first ls)) 0
    :else (let [list-index-r (list-index pred (rest ls))]
            (if (number? list-index-r)
              (+ list-index-r 1)
              false))))
(def rib-find-position list-find-position)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grammar
;; program
(define-datatype AProgram a-program [stmt])

;; statement
(define-datatype AssignStatement assign-statement [id exp])
(define-datatype PrintStatement print-statement [exp])
(define-datatype CompoundStatement compound-statement [stmts])
(define-datatype IfStatement if-statement [exp true-stmt false-stmt])
(define-datatype WhileStatement while-statement [exp stmt])
(define-datatype BlockStatement block-statement [ids body])

;; expression
(define-datatype LitExp lit-exp [datum])
(define-datatype VarExp var-exp [id])
(define-datatype PrimappExp primapp-exp [prim rands])
(define-datatype IfExp if-exp [test-exp true-exp false-exp])
(define-datatype LetExp let-exp [ids rands body])
(define-datatype ProcExp proc-exp [ids body])
(define-datatype AppExp app-exp [rator rands])
(define-datatype LetrecExp letrec-exp [proc-names idss bodies letrec-body])
(define-datatype VarassignExp varassign-exp [id rhs-exp])
(define-datatype BeginExp begin-exp [exp exps])

;; primitive
(define-datatype AddPrim add-prim [])
(define-datatype SubtractPrim subtract-prim  [])
(define-datatype MultPrim multi-prim [])
(define-datatype IncrPrim incr-prim [])
(define-datatype DecrPrim decr-prim [])
(define-datatype ZeroTestPrim zero-test-prim [])

;; procval
(define-datatype Closure closure [ids body env])

;; reference
(define-datatype ARef a-ref [position vec])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpreter
(declare execute-statement
         eval-expression eval-rands eval-rand apply-primitive apply-procval)
(defn execute-program [pgm]
  (condp instance? pgm
    AProgram (execute-statement (:stmt pgm) (empty-env))))
(defn execute-statement [stmt env]
  (condp instance? stmt
    AssignStatement (setref!
                      (apply-env-ref env (:id stmt))
                      (eval-expression (:exp stmt) env))
    PrintStatement (do
                     (print (eval-expression (:exp stmt) env))
                     (newline))
    CompoundStatement (doseq [statement (:stmts stmt)]
                        (execute-statement statement env))
    IfStatement (if (true-value? (eval-expression (:exp stmt) env))
                  (execute-statement (:true-stmt stmt) env)
                  (execute-statement (:false-stmt stmt) env))
    WhileStatement (let [loop (fn loop []
                                 (if (true-value? (eval-expression (:exp stmt) env))
                                   (do
                                     (execute-statement (:stmt stmt) env)
                                     (loop))))]
                     (loop))
    BlockStatement (execute-statement
                     (:body stmt)
                     (extend-env (:ids stmt) (map (fn [id] 0) (:ids stmt)) env))
    ))

(defn eval-expression [exp env]
  (condp instance? exp
    LitExp (:datum exp)
    VarExp (apply-env env (:id exp))
    PrimappExp (let [args (eval-rands (:rands exp) env)]
                 (apply-primitive (:prim exp) args))
    IfExp (if (true-value? (eval-expression (:test-exp exp) env))
            (eval-expression (:true-exp exp) env)
            (eval-expression (:false-exp exp) env))
    LetExp (let [args (eval-rands (:rands exp) env)]
             (eval-expression (:body exp) (extend-env (:ids exp) args env)))
    ProcExp (closure (:ids exp) (:body exp) env)
    AppExp (let [proc (eval-expression (:rator exp) env)
                 args (eval-rands (:rands exp) env)]
             (if (instance? Closure proc)
               (apply-procval proc args)
               (throw (Exception. (str 'eval-expression
                                       ": Attempt to apply non-procedure " exp)))))
    LetrecExp (eval-expression (:letrec-body exp)
                               (extend-env-recursively
                                 (:proc-names exp) (:idss exp) (:bodies exp) env))
    VarassignExp (do
                   (setref!
                     (apply-env-ref env (:id exp))
                     (eval-expression (:rhs-exp exp) env))
                   1)
    BeginExp (let [val (eval-expression (:exp exp) env)
                   vals (map (fn [exp] (eval-expression exp env)) (:exps exp))]
               (if (empty? vals)
                 val
                 (last vals)))
    ))
(defn eval-rands [rands env]
  (map (fn [x] (eval-rand x env)) rands))
(defn eval-rand [rand env]
  (eval-expression rand env))
(defn apply-primitive [prim args]
  (condp instance? prim
    AddPrim (+ (first args) (second args))
    SubtractPrim (- (first args) (second args))
    MultPrim (* (first args) (second args))
    IncrPrim (+ (first args) 1)
    DecrPrim (- (first args) 1)
    ZeroTestPrim (if (zero? (first args)) 1 0)
    ))
(defn apply-procval [proc args]
  (condp instance? proc
    Closure (eval-expression (:body proc) (extend-env (:ids proc) args (:env proc)))))
