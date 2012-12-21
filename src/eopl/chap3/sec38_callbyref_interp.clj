(ns eopl.chap3.sec38-callbyref-interp
  (:use eopl.chap3.sec38-callbyref-grammar
        eopl.chap3.sec38-callbyref-env)
  (:import (eopl.chap3.sec38_callbyref_grammar
             AProgram
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim
             Closure
             DirectTarget IndirectTarget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpreter
(declare eval-expression
         eval-primapp-exp-rands
         eval-let-exp-rands eval-let-exp-rand
         eval-rands eval-rand
         apply-primitive apply-procval)
(defn eval-program [pgm]
  (condp instance? pgm
    AProgram (eval-expression (:exp pgm) (empty-env))))
(defn eval-expression [exp env]
  (condp instance? exp
    LitExp (:datum exp)
    VarExp (apply-env env (:id exp))
    PrimappExp (let [args (eval-primapp-exp-rands (:rands exp) env)]
                 (apply-primitive (:prim exp) args))
    IfExp (if (true-value? (eval-expression (:test-exp exp) env))
            (eval-expression (:true-exp exp) env)
            (eval-expression (:false-exp exp) env))
    LetExp (let [args (eval-let-exp-rands (:rands exp) env)]
             (eval-expression (:body exp) (extend-env (:ids exp) args env)))
    ProcExp (Closure. (:ids exp) (:body exp) env)
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
(defn eval-primapp-exp-rands [rands env]
  (map (fn [x] (eval-expression x env)) rands))
(defn eval-let-exp-rands [rands env]
  (map (fn [x] (eval-let-exp-rand x env)) rands))
(defn eval-let-exp-rand [rand env]
  (DirectTarget. (eval-expression rand env)))
(defn eval-rands [rands env]
  (map (fn [x] (eval-rand x env)) rands))
(defn eval-rand [rand env]
  (condp instance? rand
    VarExp (IndirectTarget.
             (let [ref (apply-env-ref env (:id rand))
                   target (primitive-deref ref)]
               (condp instance? target
                 DirectTarget ref
                 IndirectTarget (:ref target))))
    (DirectTarget. (eval-expression rand env))))
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
