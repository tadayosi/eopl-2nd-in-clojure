(ns eopl.chap4.sec4x-type
  (:use clojure.pprint
        eopl.chap4.sec4x-grammar
        eopl.chap4.sec4x-env
        eopl.chap4.sec4x-tenv
        eopl.chap4.sec4x-parser2)
  (:import (eopl.chap4.sec4x_grammar
             AProgram
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             TrueExp FalseExp LettypeExp
             ProcTypeExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim
             AtomicType ProcType)))

;; Checking for equal types
(declare type-to-external-form arg-types-external-form)
(defn check-equal-type! [t1 t2 exp]
  (if (not= t1 t2)
    (throw (Exception. (format "[%s] Types didn't match: %s != %s in %s"
                               'check-equal-type!
                               (type-to-external-form t1)
                               (type-to-external-form t2)
                               (binding [*print-dup* true] (pr-str exp)))))))
(defn type-to-external-form [ty]
  (condp instance? ty
    AtomicType (:name ty)
    ProcType (concat
               (arg-types-external-form (:arg-types ty))
               '(->)
               (list (type-to-external-form (:result-type ty))))))
(defn arg-types-external-form [types]
  (interpose '* (map type-to-external-form types)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Checker
(declare type-of-expression types-of-expressions
         type-of-proc-exp type-of-application type-of-primitive type-of-let-exp type-of-letrec-exp type-of-lettype-exp
         fresh-type)
(defn type-of-program [pgm]
  (condp instance? pgm
    AProgram (type-of-expression (:exp pgm) (empty-tenv))))
(defn type-of-expression [exp tenv]
  (condp instance? exp
    LitExp int-type
    TrueExp bool-type
    FalseExp bool-type
    VarExp (apply-tenv tenv (:id exp))
    IfExp (let [test-type (type-of-expression (:test-exp exp) tenv)
                false-type (type-of-expression (:false-exp exp) tenv)
                true-type (type-of-expression (:true-exp exp) tenv)]
            (check-equal-type! test-type bool-type (:test-exp exp))
            (check-equal-type! true-type false-type exp)
            true-type)
    ProcExp (type-of-proc-exp (:arg-texps exp) (:ids exp) (:body exp) tenv)
    PrimappExp (type-of-application
                 (type-of-primitive (:prim exp))
                 (types-of-expressions (:rands exp) tenv)
                 (:prim exp) (:rands exp) exp)
    AppExp (type-of-application
             (type-of-expression (:rator exp) tenv)
             (types-of-expressions (:rands exp) tenv)
             (:rator exp) (:rands exp) exp)
    LetExp (type-of-let-exp (:ids exp) (:rands exp) (:body exp) tenv)
    LetrecExp (type-of-letrec-exp
                (:result-texps exp) (:proc-names exp) (:args-texpss exp) (:idss exp) (:bodies exp)
                (:letrec-body exp) tenv)
    LettypeExp (type-of-lettype-exp
                 (:type-name exp) (:texp exp) (:result-texps exp) (:proc-names exp)
                 (:arg-texpss exp) (:idss exp) (:bodies exp)
                 (:lettype-body exp) tenv)
    ))
(defn types-of-expressions [rands tenv]
  (map (fn [exp] (type-of-expression exp tenv)) rands))

(defn type-of-proc-exp [texps ids body tenv]
  (let [arg-types (expand-type-expressions texps tenv)]
    (let [result-type (type-of-expression
                        body
                        (extend-tenv ids arg-types tenv))]
      (ProcType. arg-types result-type))))
(defn type-of-application [rator-type rand-types rator rands exp]
  (condp instance? rator-type
    ProcType (if (= (count (:arg-types rator-type)) (count rand-types))
               (do
                 (doseq [[rand-type arg-type rand] (map list rand-types (:arg-types rator-type) rands)]
                   (check-equal-type! rand-type arg-type rand))
                 (:result-type rator-type))
               (throw (Exception. (str 'type-of-expression
                            ": Wrong number of arguments in expression "
                            exp ": "
                            "expected " (map type-to-external-form (:arg-types rator-type))
                            "got " (type-to-external-form rator-type)))))
    (throw (Exception. (format "[%s] Rator not a proc type: %s had rator type %s"
                               'type-of-expression
                               (binding [*print-dup* true] (pr-str rator))
                               (type-to-external-form rator-type))))))
(defn type-of-primitive [prim]
  (condp instance? prim
    AddPrim (ProcType. (list int-type int-type) int-type)
    SubtractPrim (ProcType. (list int-type int-type) int-type)
    MultPrim (ProcType. (list int-type int-type) int-type)
    IncrPrim (ProcType. (list int-type) int-type)
    DecrPrim (ProcType. (list int-type) int-type)
    ZeroTestPrim (ProcType. (list int-type) bool-type)
    ))
(defn type-of-let-exp [ids rands body tenv]
  (let [tenv-for-body (extend-tenv
                        ids
                        (types-of-expressions rands tenv)
                        tenv)]
    (type-of-expression body tenv-for-body)))
(defn type-of-letrec-exp [result-texps proc-names texpss idss bodies letrec-body tenv]
  (let [arg-typess (map
                     (fn [texps]
                       (expand-type-expressions texps tenv))
                     texpss)
        result-types (expand-type-expressions result-texps tenv)]
    (let [the-proc-types (map (fn [x y] (ProcType. x y)) arg-typess result-types)]
      (let [tenv-for-body (extend-tenv
                            proc-names the-proc-types tenv)]
        (doseq [[ids arg-types body result-type] (map list idss arg-typess bodies result-types)]
          (check-equal-type!
            (type-of-expression
              body
              (extend-tenv ids arg-types tenv-for-body))
            result-type
            body))
        (type-of-expression letrec-body tenv-for-body)))))
(defn type-of-lettype-exp [type-name texp
                           result-texps proc-names arg-texpss idss bodies
                           lettype-body tenv]
  (let [the-new-type (fresh-type type-name)
        rhs-texps (map
                    (fn [x y] (ProcTypeExp. x y)) arg-texpss result-texps)]
    (let [tenv-for-implementation (extend-tenv-with-typedef-exp
                                    type-name texp tenv)
          tenv-for-client (extend-tenv-with-typedef
                            type-name the-new-type tenv)]
      (let [tenv-for-proc (extend-tenv-with-type-exps
                            proc-names rhs-texps tenv-for-implementation)
            tenv-for-body (extend-tenv-with-type-exps
                            proc-names rhs-texps tenv-for-client)]
        (doseq [[ids arg-texps body result-texp]
                (map list idss arg-texpss bodies result-texps)]
          (check-equal-type!
            (type-of-expression
              body
              (extend-tenv-with-type-exps
                ids arg-texps tenv-for-proc))
            (expand-type-expression
              result-texp tenv-for-proc)
            body))
        (type-of-expression lettype-body tenv-for-body)))))
(def fresh-type
  (let [counter (atom 0)]
    (fn [s]
      (swap! counter + 1)
      (AtomicType. (symbol (str s @counter))))))

;; Top level
(defn type-check [string]
  (type-to-external-form
    (type-of-program
      (scan&parse string))))
