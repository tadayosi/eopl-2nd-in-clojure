(ns eopl.chap2.sec22-test
  (:use clojure.test
        eopl.chap2.sec22))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.1 define-datatype and cases
;; bintree
(deftest test-leaf-sum
  (is (= 1 (leaf-sum (leaf-node 1))))
  (is (= 3 (leaf-sum (interior-node
                       "a"
                       (interior-node "b" (leaf-node 1) (leaf-node 1))
                       (leaf-node 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.2 Abstract Syntax and its Representation
;; occurs-free?
(deftest test-occurs-free?
  (is (= true (occurs-free? 'x (var-exp 'x))))
  (is (= false (occurs-free? 'y (var-exp 'x))))
  (is (= false (occurs-free?
                 'x
                 ; (lambda (x) (f (f x)))
                 (lambda-exp 'x (app-exp (var-exp 'f) (app-exp (var-exp 'f) (var-exp 'x))))))))

;; unparse-expression
(deftest test-unparse-expression
  (is (= 'x (unparse-expression (var-exp 'x))))
  (is (= '(lambda (x) (f (f x)))
         (unparse-expression
           (lambda-exp 'x (app-exp (var-exp 'f) (app-exp (var-exp 'f) (var-exp 'x))))))))

;; parse-expression
(deftest test-parse-expression
  (is (= (var-exp 'x) (parse-expression 'x)))
  (is (= (lambda-exp 'x (app-exp (var-exp 'f) (app-exp (var-exp 'f) (var-exp 'x))))
         (parse-expression '(lambda (x) (f (f x))))))
  (is (thrown? Exception (parse-expression nil))))
