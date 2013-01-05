(ns eopl.chap3.sec31_7-callbyval-parser2
  (:use eopl.chap3.sec31_7-callbyval-grammar
        eopl.chap3.sec31_7-callbyval-interp)
  (:import (eopl.chap3.sec31_7_callbyval_grammar
             AProgram
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim)
           (org.antlr.runtime ANTLRStringStream
                              CommonTokenStream)
           (eopl.chap3 Sec31_7Lexer Sec31_7Parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(declare parse-expression parse-rands parse-rand parse-primitive
         parse-ids parse-id parse-number parse-proc-decls)
(defn parse-program [pgm]
  (let [lexer (Sec31_7Lexer. (ANTLRStringStream. pgm))
        parser (Sec31_7Parser. (CommonTokenStream. lexer))]
    (let [ast (-> parser .program .getTree)]
      (condp = (.getType ast)
        (Sec31_7Parser/A_PROGRAM) (AProgram. (parse-expression (.getChild ast 0)))))))
(defn parse-expression [exp]
  (condp = (.getType exp)
    (Sec31_7Parser/LIT_EXP) (LitExp. (parse-number (.getChild exp 0)))
    (Sec31_7Parser/VAR_EXP) (VarExp. (parse-id (.getChild exp 0)))
    (Sec31_7Parser/PRIMAPP_EXP) (PrimappExp. (parse-primitive (.getChild exp 0))
                                             (parse-rands (.getChild exp 1)))
    (Sec31_7Parser/IF_EXP) (IfExp. (parse-expression (.getChild exp 0))
                                   (parse-expression (.getChild exp 1))
                                   (parse-expression (.getChild exp 2)))
    (Sec31_7Parser/LET_EXP) (LetExp. (parse-ids (.getChild exp 0))
                                     (parse-rands (.getChild exp 1))
                                     (parse-expression (.getChild exp 2)))
    (Sec31_7Parser/PROC_EXP) (ProcExp. (parse-ids (.getChild exp 0))
                                       (parse-expression (.getChild exp 1)))
    (Sec31_7Parser/APP_EXP) (AppExp. (parse-expression (.getChild exp 0))
                                     (parse-rands (.getChild exp 1)))
    (Sec31_7Parser/LETREC_EXP) (let [[proc-names idss bodies] (parse-proc-decls
                                                                (.getChild exp 0))]
                                 (LetrecExp.
                                   proc-names idss bodies
                                   (parse-expression (.getChild exp 1))))
    (Sec31_7Parser/VARASSIGN_EXP) (VarassignExp.
                                    (parse-id (.getChild exp 0))
                                    (parse-expression (.getChild exp 1)))
    (Sec31_7Parser/BEGIN_EXP) (BeginExp. (parse-expression (.getChild exp 0))
                                         (for [exp (-> exp (.getChild 1) .getChildren)]
                                           (parse-expression exp)))
    (throw (Exception. (str 'parse-expression
                            ": Invalid concrete syntax " exp)))))
(defn parse-rands [rands]
  (for [rand (.getChildren rands)] (parse-rand rand)))
(defn parse-rand [rand]
  (parse-expression rand))
(defn parse-primitive [prim]
  (condp = (.getText prim)
    "+" (AddPrim.)
    "-" (SubtractPrim.)
    "*" (MultPrim.)
    "add1" (IncrPrim.)
    "sub1" (DecrPrim.)
    "zero?" (ZeroTestPrim.)
    (throw (Exception. (str 'parse-primitive
                            ": Invalid concrete syntax " prim)))))
(defn parse-number [num]
  (-> num .getText read-string))
(defn parse-ids [ids]
  (for [id (.getChildren ids)] (parse-id id)))
(defn parse-id [id]
  (-> id .getText symbol))
(defn parse-proc-decls [proc-decls]
  (apply map list
         (for [proc-decl (.getChildren proc-decls)]
           [(parse-id (.getChild proc-decl 0))
            (parse-ids (.getChild proc-decl 1))
            (parse-expression (.getChild proc-decl 2))])))

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
