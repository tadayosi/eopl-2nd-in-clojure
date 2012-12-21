(ns eopl.chap3.sec38-callbyneed-test
  (:use clojure.test
        eopl.chap3.sec38-callbyneed-grammar
        eopl.chap3.sec38-callbyneed-env
        eopl.chap3.sec38-callbyneed-interp
        eopl.chap3.sec38-callbyneed-parser)
  (:import (eopl.chap3.sec38_callbyneed_grammar
             LitExp VarExp PrimappExp IfExp AddPrim SubtractPrim IncrPrim
             DirectTarget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
(deftest test-env
  (is (thrown? Exception (apply-env (empty-env) 'x)))
  (is (= 6 (apply-env (extend-env '(d x y) (map (fn [x] (DirectTarget. x)) '(6 7 8)) (empty-env)) 'd)))
  (is (= 7 (apply-env (extend-env '(d x y) (map (fn [x] (DirectTarget. x)) '(6 7 8)) (empty-env)) 'x)))
  (is (= 8 (apply-env (extend-env '(d x y) (map (fn [x] (DirectTarget. x)) '(6 7 8)) (empty-env)) 'y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grammar
(deftest test-true-value?
  (is (= true (true-value? 1)))
  (is (= false (true-value? 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpreter
(defn init-env []
  (extend-env
    '(i v x)
    (map (fn [x] (DirectTarget. x)) '(1 5 10))
    (empty-env)))
(deftest test-eval-expression
  (is (= 14 (eval-expression (PrimappExp.
                               (IncrPrim.)
                               (list (PrimappExp.
                                       (AddPrim.)
                                       (list (LitExp. 3)
                                             (VarExp. 'x)))))
                             (init-env))))
  (is (= 2 (eval-expression (IfExp.
                              (LitExp. 1)
                              (LitExp. 2)
                              (LitExp. 3))
                            (init-env))))
  (is (= 3 (eval-expression (IfExp.
                              (PrimappExp.
                                (SubtractPrim.)
                                (list (LitExp. 3)
                                      (PrimappExp.
                                        (AddPrim.)
                                        (list (LitExp. 1)
                                              (LitExp. 2)))))
                              (LitExp. 2)
                              (LitExp. 3))
                            (init-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(deftest test-run
  (is (= 5 (run '5)))
  (is (= 3 (run '(add1 2))))
  (is (= 5 (run '(+ (add1 2 ) (- 6 4)))))
  (is (= 2 (run '(if 1 2 3))))
  (is (= 3 (run '(if (- 3 (+ 1 2)) 2 3))))
  (is (= 11 (run '(let (x y) (5 6) (+ x y)))))
  (is (= 4 (run '(let (x) (1) (let (x) ((+ x 2)) (add1 x))))))
  (is (= 2 (run '((proc (x) (add1 x)) 1))))
  (is (= 176 (run '(let (x) (5)
                     (let (x f g)
                       (38
                         (proc (y z) (* y (+ x z)))
                         (proc (u) (+ u x)))
                       (f (g 3) 17))))))
  (is (= 1 (run '(zero? 0))))
  (is (= 0 (run '(zero? (sub1 5)))))
  (is (= 720 (run '(letrec
                   (fact) ((x)) ((if (zero? x) 1 (* x (fact (sub1 x)))))
                   (fact 6)))))
  (is (= 1 (run '(letrec
                   (even odd) ((x) (x)) ((if (zero? x) 1 (odd (sub1 x)))
                                          (if (zero? x) 0 (even (sub1 x))))
                   (odd 13)))))
  (is (= 1 (run '(let (x) (0)
                   (letrec
                     (even odd) (() ()) ((if (zero? x)
                                           1
                                           (let (d) ((set x (sub1 x)))
                                             (odd)))
                                          (if (zero? x)
                                            0
                                            (let (d) ((set x (sub1 x)))
                                              (even))))
                     (let (d) ((set x 13))
                       (odd)))))))
  (is (= 3 (run '(let (g) ((let (count) (0)
                             (proc ()
                                   (let (d) ((set count (add1 count)))
                                     count))))
                   (+ (g) (g))))))
  (is (= 203 (run '(let (x) (100)
                     (let (p) ((proc (x) (let (d) ((set x (add1 x)))
                                           x)))
                       (+ (p x) (p x)))))))
  (is (= 1 (run '(begin 1))))
  (is (= 3 (run '(begin 1 2 (+ 1 2)))))
  (is (= 1 (run '(let (a b swap) (3
                                    4
                                    (proc (x y)
                                          (let (temp) (x)
                                            (begin
                                              (set x y)
                                              (set y temp)))))
                    (begin
                      (swap a b)
                      (- a b))))))
  (is (= 2 (run '(let (g) ((let (count) (0)
                             (proc ()
                                   (begin
                                     (set count (add1 count))
                                     count))))
                   ((proc (x) (+ x x))
                     (g))))))
  (is (= 1 (run '(let (count) (0)
                   (begin
                     ((proc (a b)
                            ((proc (x)
                                   ((proc (y)
                                          ((proc (z) (+ (+ x y) z)) y))
                                     x))
                              (begin
                                (set count (add1 count))
                                (+ a b))))
                       15 20)
                     count)))))
  )
