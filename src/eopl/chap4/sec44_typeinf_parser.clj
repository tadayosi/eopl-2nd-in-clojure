(ns eopl.chap4.sec44-typeinf-parser
  (:use eopl.chap4.sec44-typeinf-grammar)
  (:import (eopl.chap4.sec44_typeinf_grammar
             AProgram
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             TrueExp FalseExp LettypeExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim
             IntTypeExp BoolTypeExp ProcTypeExp TidTypeExp
             ATypeExp NoTypeExp)
           (org.antlr.runtime ANTLRStringStream
                              CommonTokenStream)
           (eopl.chap4 Sec44Lexer Sec44Parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(declare parse-expression parse-rands parse-rand parse-primitive
         parse-number parse-ids parse-id
         parse-typed-ids parse-optional-type-exp parse-type-exp parse-arg-type-exps
         parse-proc-decls)
(defn scan&parse [pgm]
  (let [lexer (Sec44Lexer. (ANTLRStringStream. pgm))
        parser (Sec44Parser. (CommonTokenStream. lexer))]
    (let [ast (-> parser .program .getTree)]
      (condp = (.getType ast)
        (Sec44Parser/A_PROGRAM) (AProgram. (parse-expression (.getChild ast 0)))))))
(defn parse-expression [exp]
  (condp = (.getType exp)
    (Sec44Parser/LIT_EXP) (LitExp. (parse-number (.getChild exp 0)))
    (Sec44Parser/VAR_EXP) (VarExp. (parse-id (.getChild exp 0)))
    (Sec44Parser/PRIMAPP_EXP) (PrimappExp. (parse-primitive (.getChild exp 0))
                                           (parse-rands (.getChild exp 1)))
    (Sec44Parser/IF_EXP) (IfExp. (parse-expression (.getChild exp 0))
                                 (parse-expression (.getChild exp 1))
                                 (parse-expression (.getChild exp 2)))
    (Sec44Parser/LET_EXP) (LetExp. (parse-ids (.getChild exp 0))
                                   (parse-rands (.getChild exp 1))
                                   (parse-expression (.getChild exp 2)))
    (Sec44Parser/PROC_EXP) (let [[optional-arg-texps ids] (parse-typed-ids
                                                            (.getChild exp 0))]
                             (ProcExp. optional-arg-texps ids
                                       (parse-expression (.getChild exp 1))))
    (Sec44Parser/APP_EXP) (AppExp. (parse-expression (.getChild exp 0))
                                   (parse-rands (.getChild exp 1)))
    (Sec44Parser/LETREC_EXP) (let [[optional-result-texps proc-names
                                    optional-args-texpss idss bodies] (parse-proc-decls
                                                                        (.getChild exp 0))]
                               (LetrecExp.
                                 optional-result-texps proc-names
                                 optional-args-texpss idss bodies
                                 (parse-expression (.getChild exp 1))))
    (Sec44Parser/VARASSIGN_EXP) (VarassignExp.
                                  (parse-id (.getChild exp 0))
                                  (parse-expression (.getChild exp 1)))
    (Sec44Parser/BEGIN_EXP) (BeginExp. (parse-expression (.getChild exp 0))
                                       (for [exp (-> exp (.getChild 1) .getChildren)]
                                         (parse-expression exp)))
    (Sec44Parser/TRUE_EXP) (TrueExp.)
    (Sec44Parser/FALSE_EXP) (FalseExp.)
    (Sec44Parser/LETTYPE_EXP) (let [[optional-result-texps proc-names
                                     optional-args-texpss idss bodies] (parse-proc-decls
                                                                         (.getChild exp 2))]
                                (LettypeExp.
                                  (parse-id (.getChild exp 0))
                                  (parse-type-exp (.getChild exp 1))
                                  optional-result-texps proc-names
                                  optional-args-texpss idss bodies
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
             [(parse-optional-type-exp (.getChild typed-id 0))
              (parse-id (.getChild typed-id 1))]))))
(defn parse-optional-type-exp [texp]
  (condp = (.getType texp)
    (Sec44Parser/A_TYPE_EXP) (ATypeExp. (parse-type-exp (.getChild texp 0)))
    (Sec44Parser/NO_TYPE_EXP) (NoTypeExp.)))
(defn parse-type-exp [texp]
  (condp = (.getType texp)
    (Sec44Parser/INT_TYPE_EXP) (IntTypeExp.)
    (Sec44Parser/BOOL_TYPE_EXP) (BoolTypeExp.)
    (Sec44Parser/PROC_TYPE_EXP) (ProcTypeExp.
                                  (parse-arg-type-exps (.getChild texp 0))
                                  (ATypeExp. (parse-type-exp (.getChild texp 1))))
    (Sec44Parser/TID_TYPE_EXP) (TidTypeExp.
                                 (parse-id (.getChild texp 0)))
    (throw (Exception. (str 'parse-type-exp
                            ": Invalid type expression " (.getText texp))))))
(defn parse-arg-type-exps [arg-texps]
  (for [arg-texp (.getChildren arg-texps)] (ATypeExp. (parse-type-exp arg-texp))))
(defn parse-proc-decls [proc-decls]
  (if (empty? (.getChildren proc-decls))
    ['() '()]
    (apply map list
           (for [proc-decl (.getChildren proc-decls)]
             (let [[arg-texps ids] (parse-typed-ids (.getChild proc-decl 2))]
               [(parse-optional-type-exp (.getChild proc-decl 0))
                (parse-id (.getChild proc-decl 1))
                arg-texps
                ids
                (parse-expression (.getChild proc-decl 3))])))))
