(ns eopl.chap3.sec31_7-callbyval-parser
  (:use eopl.chap3.sec31_7-callbyval-grammar
        eopl.chap3.sec31_7-callbyval-interp)
  (:import (eopl.chap3.sec31_7_callbyval_grammar
             AProgram
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(declare parse-expression parse-rands parse-rand parse-primitive)
(defn parse-program [pgm]
  (AProgram. (parse-expression pgm)))
(defn parse-expression [exp]
  (cond
    (number? exp) (LitExp. exp)
    (symbol? exp) (VarExp. exp)
    (list? exp) (condp = (first exp)
                  'if (IfExp. (parse-expression (nth exp 1))
                              (parse-expression (nth exp 2))
                              (parse-expression (nth exp 3)))
                  'let (LetExp. (nth exp 1)
                                (parse-rands (nth exp 2))
                                (parse-expression (nth exp 3)))
                  'proc (ProcExp. (nth exp 1)
                                  (parse-expression (nth exp 2)))
                  'letrec (LetrecExp.
                            (nth exp 1) (nth exp 2) (parse-rands (nth exp 3))
                            (parse-expression (nth exp 4)))
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
