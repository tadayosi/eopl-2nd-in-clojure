(ns eopl.chap3.sec38-callbyneed-parser
  (:use eopl.chap3.sec38-callbyneed-interp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(declare parse-expression parse-rands parse-rand parse-primitive)
(defn parse-program [pgm]
  (a-program (parse-expression pgm)))
(defn parse-expression [exp]
  (cond
    (number? exp) (lit-exp exp)
    (symbol? exp) (var-exp exp)
    (list? exp) (condp = (first exp)
                  'if (if-exp (parse-expression (nth exp 1))
                              (parse-expression (nth exp 2))
                              (parse-expression (nth exp 3)))
                  'let (let-exp (nth exp 1)
                         (parse-rands (nth exp 2))
                         (parse-expression (nth exp 3)))
                  'proc (proc-exp (nth exp 1)
                                  (parse-expression (nth exp 2)))
                  'letrec (letrec-exp
                            (nth exp 1) (nth exp 2) (parse-rands (nth exp 3))
                            (parse-expression (nth exp 4)))
                  'set (varassign-exp
                         (nth exp 1) (parse-expression (nth exp 2)))
                  'begin (begin-exp (parse-expression (nth exp 1))
                                    (map (fn [exp] (parse-expression exp)) (rest (rest exp))))
                  (if (.contains (list '+ '- '* 'add1 'sub1 'zero?) (first exp))
                    (primapp-exp (parse-primitive (first exp))
                                 (parse-rands (rest exp)))
                    (app-exp (parse-expression (first exp))
                             (parse-rands (rest exp)))))
    :else (throw (Exception. (str 'parse-expression
                                  ": Invalid concrete syntax " exp)))))
(defn parse-rands [rands]
  (map (fn [x] (parse-rand x)) rands))
(defn parse-rand [rand]
  (parse-expression rand))
(defn parse-primitive [prim]
  (condp = prim
    '+ (add-prim)
    '- (subtract-prim)
    '* (multi-prim)
    'add1 (incr-prim)
    'sub1 (decr-prim)
    'zero? (zero-test-prim)
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
