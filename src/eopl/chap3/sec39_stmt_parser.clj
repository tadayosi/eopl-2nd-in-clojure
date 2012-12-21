(ns eopl.chap3.sec39-stmt-parser
  (:use eopl.chap3.sec39-stmt-grammar
        eopl.chap3.sec39-stmt-interp)
  (:import (eopl.chap3.sec39_stmt_grammar
             AProgram
             AssignStatement PrintStatement CompoundStatement IfStatement WhileStatement BlockStatement
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(declare parse-statement
         parse-expression parse-rands parse-rand parse-primitive)
(defn parse-program [pgm]
  (AProgram. (parse-statement pgm)))
(defn parse-statement [stmt]
  (condp = (first stmt)
    '= (AssignStatement. (nth stmt 1)
                         (parse-expression (nth stmt 2)))
    'print (PrintStatement. (parse-expression (nth stmt 1)))
    'if (IfStatement. (parse-expression (nth stmt 1))
                      (parse-statement (nth stmt 2))
                      (parse-statement (nth stmt 3)))
    'while (WhileStatement. (parse-expression (nth stmt 1))
                            (parse-statement (nth stmt 2)))
    'var (BlockStatement. (nth stmt 1)
                          (parse-statement (nth stmt 2)))
    (if (list? (first stmt))
      (CompoundStatement. (map parse-statement stmt))
      (throw (Exception. (str 'parse-statement
                              ": Invalid concrete syntax " stmt))))))

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
  (execute-program (parse-program x)))
(defn read-eval-print []
  (do
    (println "--> ")
    (println (execute-program (parse-program (read))))
    (newline)
    (read-eval-print)))
#_(read-eval-print)
