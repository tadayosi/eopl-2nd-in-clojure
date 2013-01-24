(ns eopl.chap4.sec4x-parser2
  (:use eopl.chap4.sec4x-grammar
        eopl.chap4.sec4x-interp)
  (:import (eopl.chap4.sec4x_grammar
             AProgram
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             TrueExp FalseExp LettypeExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim
             IntTypeExp BoolTypeExp ProcTypeExp TidTypeExp)
           (org.antlr.runtime ANTLRStringStream
                              CommonTokenStream)
           (eopl.chap4 Sec4xLexer Sec4xParser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(declare parse-expression parse-rands parse-rand parse-primitive
         parse-number parse-ids parse-id
         parse-typed-ids parse-type-exp parse-arg-type-exps
         parse-proc-decls)
(defn scan&parse [pgm]
  (let [lexer (Sec4xLexer. (ANTLRStringStream. pgm))
        parser (Sec4xParser. (CommonTokenStream. lexer))]
    (let [ast (-> parser .program .getTree)]
      (condp = (.getType ast)
        (Sec4xParser/A_PROGRAM) (AProgram. (parse-expression (.getChild ast 0)))))))
(defn parse-expression [exp]
  (condp = (.getType exp)
    (Sec4xParser/LIT_EXP) (LitExp. (parse-number (.getChild exp 0)))
    (Sec4xParser/VAR_EXP) (VarExp. (parse-id (.getChild exp 0)))
    (Sec4xParser/PRIMAPP_EXP) (PrimappExp. (parse-primitive (.getChild exp 0))
                                           (parse-rands (.getChild exp 1)))
    (Sec4xParser/IF_EXP) (IfExp. (parse-expression (.getChild exp 0))
                                 (parse-expression (.getChild exp 1))
                                 (parse-expression (.getChild exp 2)))
    (Sec4xParser/LET_EXP) (LetExp. (parse-ids (.getChild exp 0))
                                   (parse-rands (.getChild exp 1))
                                   (parse-expression (.getChild exp 2)))
    (Sec4xParser/PROC_EXP) (let [[arg-texps ids] (parse-typed-ids
                                                   (.getChild exp 0))]
                             (ProcExp. arg-texps ids 
                                       (parse-expression (.getChild exp 1))))
    (Sec4xParser/APP_EXP) (AppExp. (parse-expression (.getChild exp 0))
                                   (parse-rands (.getChild exp 1)))
    (Sec4xParser/LETREC_EXP) (let [[result-texps proc-names
                                    args-texpss idss bodies] (parse-proc-decls
                                                               (.getChild exp 0))]
                               (LetrecExp.
                                 result-texps proc-names
                                 args-texpss idss bodies
                                 (parse-expression (.getChild exp 1))))
    (Sec4xParser/VARASSIGN_EXP) (VarassignExp.
                                  (parse-id (.getChild exp 0))
                                  (parse-expression (.getChild exp 1)))
    (Sec4xParser/BEGIN_EXP) (BeginExp. (parse-expression (.getChild exp 0))
                                       (for [exp (-> exp (.getChild 1) .getChildren)]
                                         (parse-expression exp)))
    (Sec4xParser/TRUE_EXP) (TrueExp.)
    (Sec4xParser/FALSE_EXP) (FalseExp.)
    (Sec4xParser/LETTYPE_EXP) (let [[result-texps proc-names
                                     args-texpss idss bodies] (parse-proc-decls
                                                                (.getChild exp 2))]
                                (LettypeExp.
                                  (parse-id (.getChild exp 0))
                                  (parse-type-exp (.getChild exp 1))
                                  result-texps proc-names
                                  args-texpss idss bodies
                                  (parse-expression (.getChild exp 3))))
    (throw (Exception. (str 'parse-expression
                            ": Invalid concrete syntax " (.getType exp)
                            " \"" (.getText exp) "\"")))))
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
                            ": Invalid concrete syntax " (.getText prim))))))
(defn parse-number [num]
  (-> num .getText read-string))
(defn parse-ids [ids]
  (for [id (.getChildren ids)] (parse-id id)))
(defn parse-id [id]
  (-> id .getText symbol))
(defn parse-typed-ids [typed-ids]
  (if (empty? (.getChildren typed-ids))
    ['() '()]
    (apply map list
           (for [typed-id (.getChildren typed-ids)]
             [(parse-type-exp (.getChild typed-id 0))
              (parse-id (.getChild typed-id 1))]))))
(defn parse-type-exp [texp]
  (condp = (.getType texp)
    (Sec4xParser/INT_TYPE_EXP) (IntTypeExp.)
    (Sec4xParser/BOOL_TYPE_EXP) (BoolTypeExp.)
    (Sec4xParser/PROC_TYPE_EXP) (ProcTypeExp.
                                  (parse-arg-type-exps (.getChild texp 0))
                                  (parse-type-exp (.getChild texp 1)))
    (Sec4xParser/TID_TYPE_EXP) (TidTypeExp.
                                 (parse-id (.getChild texp 0)))
    (throw (Exception. (str 'parse-type-exp
                            ": Invalid type expression " (.getText texp))))))
(defn parse-arg-type-exps [arg-texps]
  (for [arg-texp (.getChildren arg-texps)] (parse-type-exp arg-texp)))
(defn parse-proc-decls [proc-decls]
  (if (empty? (.getChildren proc-decls))
    ['() '()]
    (apply map list
           (for [proc-decl (.getChildren proc-decls)]
             (let [[arg-texps ids] (parse-typed-ids (.getChild proc-decl 2))]
               [(parse-type-exp (.getChild proc-decl 0))
                (parse-id (.getChild proc-decl 1))
                arg-texps
                ids
                (parse-expression (.getChild proc-decl 3))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read-eval-print loop
(defn run [x]
  (eval-program (scan&parse x)))
(defn read-eval-print []
  (do
    (println "--> ")
    (println (eval-program (scan&parse (read))))
    (newline)
    (read-eval-print)))
#_(read-eval-print)
