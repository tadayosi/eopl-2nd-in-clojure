(ns eopl.chap3.sec39-stmt-test
  (:use clojure.test
        eopl.chap3.sec39-stmt-interp
        eopl.chap3.sec39-stmt-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
(deftest test-env
  (is (thrown? Exception (apply-env (empty-env) 'x)))
  (is (= 6 (apply-env (extend-env '(d x y) '(6 7 8) (empty-env)) 'd)))
  (is (= 7 (apply-env (extend-env '(d x y) '(6 7 8) (empty-env)) 'x)))
  (is (= 8 (apply-env (extend-env '(d x y) '(6 7 8) (empty-env)) 'y))))

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
    '(1 5 10)
    (empty-env)))
(deftest test-eval-expression
  (is (= 14 (eval-expression (primapp-exp
                               (incr-prim)
                               (list (primapp-exp
                                       (add-prim)
                                       (list (lit-exp 3)
                                             (var-exp 'x)))))
                             (init-env))))
  (is (= 2 (eval-expression (if-exp
                              (lit-exp 1)
                              (lit-exp 2)
                              (lit-exp 3))
                            (init-env))))
  (is (= 3 (eval-expression (if-exp
                              (primapp-exp
                                (subtract-prim)
                                (list (lit-exp 3)
                                      (primapp-exp
                                        (add-prim)
                                        (list (lit-exp 1)
                                              (lit-exp 2)))))
                              (lit-exp 2)
                              (lit-exp 3))
                            (init-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(deftest test-run
  ; prints 7
  (is (nil? (run '(var (x y) ((= x 3) (= y 4) (print (+ x y)))))))
  ; prints 12
  (is (nil? (run '(var (x y z) ((= x 3) (= y 4) (= z 0)
                                        (while x ((= z (+ z y)) (= x (sub1 x))))
                                        (print z))))))
  ; prints 3, 4, 3
  (is (nil? (run '(var (x) ((= x 3) (print x)
                                    (var (x) ((= x 4) (print x)))
                                    (print x))))))
  ; prints 12
  (is (nil? (run '(var (f x) ((= f (proc (x y) (* x y)))
                               (= x 3)
                               (print (f 4 x)))))))
  )
