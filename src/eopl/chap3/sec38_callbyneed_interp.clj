(ns eopl.chap3.sec38-callbyneed-interp
  (:use eopl.common))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment
(define-datatype EmptyEnvRecord empty-env-record [])
(define-datatype ExtendedEnvRecord extended-env-record [syms vals env])
#_(define-datatype RecursivelyExtendedEnvRecord recursively-extended-env-record
  [proc-names idss bodies env])

(declare apply-env apply-env-ref list-find-position list-index rib-find-position)
(declare closure direct-target a-ref deref_ setref!)
(defn empty-env []
  (empty-env-record))
(defn extend-env [syms vals env]
  (extended-env-record syms (atom (vec vals)) env))
;; Figure 3.9
#_(defn extend-env-recursively [proc-names idss bodies old-env]
  (let [rec-env (atom nil)]
    (do
      (reset! rec-env
              (fn [sym]
                (let [pos (list-find-position sym proc-names)]
                  (if (number? pos)
                    (closure
                      (nth idss pos)
                      (nth bodies pos)
                      @rec-env)
                    (apply-env old-env sym)))))
      @rec-env)))
;; Figure 3.10
#_(defn extend-env-recursively [proc-names idss bodies old-env]
  (recursively-extended-env-record
    proc-names idss bodies old-env))
;; Figure 3.12
(defn extend-env-recursively [proc-names idss bodies old-env]
  (let [len (count proc-names)
        vec (atom [])
        env (extended-env-record
              proc-names vec old-env)]
    (do
      (doseq [pos (range len)]
        (swap! vec conj (direct-target (closure
                                         (nth idss pos)
                                         (nth bodies pos)
                                         env))))
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
(define-datatype AProgram a-program [exp])

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
;; target
(define-datatype DirectTarget direct-target [expval])
(define-datatype IndirectTarget indirect-target [ref])
(define-datatype ThunkTarget thunk-target [exp env])

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
             IndirectTarget false
             ThunkTarget true))))
;; reference
(defn primitive-deref [ref]
  (condp instance? ref
    ARef (@(:vec ref) (:position ref))))
(defn primitive-setref! [ref val]
  (condp instance? ref
    ARef (swap! (:vec ref) assoc (:position ref) val)))
(declare eval-thunk eval-expression)
(defn deref_ [ref]
  (let [target (primitive-deref ref)]
    (condp instance? target
      DirectTarget (:expval target)
      IndirectTarget (let [target1 (primitive-deref (:ref target))]
                       (condp instance? target1
                         DirectTarget (:expval target1)
                         IndirectTarget (throw (Exception.
                                                 (str 'deref
                                                      ": Illegal reference: " (:ref target))))
                         ThunkTarget (eval-thunk (:ref target))))
      ThunkTarget (eval-thunk ref))))
(defn eval-thunk [ref]
  (let [target (primitive-deref ref)]
    (condp instance? target
      ThunkTarget (let [val (eval-expression (:exp target) (:env target))]
                    (primitive-setref! ref (direct-target val))
                    val)
      (throw (Exception. (str 'eval-thunk ": Impossible!"))))))
(defn setref! [ref expval]
  (let [ref (let [target (primitive-deref ref)]
              (condp instance? target
                DirectTarget ref
                IndirectTarget (:ref target)
                ThunkTarget ref))]
    (primitive-setref! ref (direct-target expval))))

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
(defn eval-primapp-exp-rands [rands env]
  (map (fn [x] (eval-expression x env)) rands))
(defn eval-let-exp-rands [rands env]
  (map (fn [x] (eval-let-exp-rand x env)) rands))
(defn eval-let-exp-rand [rand env]
  (direct-target (eval-expression rand env)))
(defn eval-rands [rands env]
  (map (fn [x] (eval-rand x env)) rands))
(defn eval-rand [rand env]
  (condp instance? rand
    VarExp (indirect-target
             (let [ref (apply-env-ref env (:id rand))
                   target (primitive-deref ref)]
               (condp instance? target
                 DirectTarget ref
                 IndirectTarget (:ref target)
                 ThunkTarget ref)))
    LitExp (direct-target (:datum rand))
    ProcExp (direct-target
              (closure (:ids rand) (:body rand) env))
    (thunk-target rand env)))
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
