(ns eopl.chap4.sec4x-parser
  (:use eopl.chap4.sec4x-grammar
        eopl.chap4.sec4x-interp)
  (:import (eopl.chap4.sec4x_grammar
             AProgram
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             TrueExp FalseExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim
             IntTypeExp BoolTypeExp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(declare parse-expression parse-rands parse-rand parse-primitive
         parse-type-exps parse-type-exp)
(defn parse-program [pgm]
  (AProgram. (parse-expression pgm)))
(defn parse-expression [exp]
  (cond
    (number? exp) (LitExp. exp)
    (symbol? exp) (VarExp. exp)
    (= 'true exp) (TrueExp.)
    (= 'false exp) (FalseExp.)
    (list? exp) (condp = (first exp)
                  'if (IfExp. (parse-expression (nth exp 1))
                              (parse-expression (nth exp 2))
                              (parse-expression (nth exp 3)))
                  'let (LetExp. (nth exp 1)
                                (parse-rands (nth exp 2))
                                (parse-expression (nth exp 3)))
                  'proc (ProcExp. (parse-type-exps (nth exp 1))
                                  (nth exp 2)
                                  (parse-expression (nth exp 3)))
                  'letrec (LetrecExp.
                            (parse-type-exps (nth exp 1)) (nth exp 2)
                            (map parse-type-exps (nth exp 3)) (nth exp 4) (parse-rands (nth exp 5))
                            (parse-expression (nth exp 6)))
                  'set (VarassignExp.
                         (nth exp 1) (parse-expression (nth exp 2)))
                  'begin (BeginExp. (parse-expression (nth exp 1))
                                    (map (fn [exp] (parse-expression exp)) (rest (rest exp))))
                  (if (.contains (list '+ '- '* 'add1 'sub1 'zero?) (first exp))
                    (PrimappExp. (parse-primitive (first exp))
                                 (parse-rands (rest exp)))
                    (AppExp. (parse-expression (first exp))
                             (parse-rands (rest exp)))))
    :else (throw (Exception. (str 'parse-expression
                                  ": Invalid concrete syntax " exp)))))
(defn parse-rands [rands]
  (map (fn [x] (parse-rand x)) rands))
(defn parse-rand [rand]
  (parse-expression rand))
(defn parse-primitive [prim]
  (condp = prim
    '+ (AddPrim.)
    '- (SubtractPrim.)
    '* (MultPrim.)
    'add1 (IncrPrim.)
    'sub1 (DecrPrim.)
    'zero? (ZeroTestPrim.)
    (throw (Exception. (str 'parse-primitive
                            ": Invalid concrete syntax " prim)))))
(defn parse-type-exps [texps]
  (map (fn [x] (parse-type-exp x)) texps))
(defn parse-type-exp [texp]
  (condp = texp
    'int (IntTypeExp.)
    'bool (BoolTypeExp.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read-eval-print loop
(defn run [x]
  (eval-program (parse-program x)))
(defn read-eval-print []
  (do
    (println "--> ")
    (println (eval-program (parse-program (read))))
    (newline)
    (read-eval-print)))
#_(read-eval-print)
