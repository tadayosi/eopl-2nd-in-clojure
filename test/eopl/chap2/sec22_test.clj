(ns eopl.chap2.sec22-test
  (:use clojure.test
        eopl.chap2.sec22)
  (:import (eopl.chap2.sec22 LeafNode InteriorNode
                             VarExp LambdaExp AppExp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.1 define-datatype and cases
;; bintree
(deftest test-leaf-sum
  (is (= 1 (leaf-sum (LeafNode. 1))))
  (is (= 3 (leaf-sum (InteriorNode.
                       "a"
                       (InteriorNode. "b" (LeafNode. 1) (LeafNode. 1))
                       (LeafNode. 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.2 Abstract Syntax and its Representation
;; occurs-free?
(deftest test-occurs-free?
  (is (= true (occurs-free? 'x (VarExp. 'x))))
  (is (= false (occurs-free? 'y (VarExp. 'x))))
  (is (= false (occurs-free?
                 'x
                 ; (lambda (x) (f (f x)))
                 (LambdaExp. 'x (AppExp. (VarExp. 'f) (AppExp. (VarExp. 'f) (VarExp. 'x))))))))

;; unparse-expression
(deftest test-unparse-expression
  (is (= 'x (unparse-expression (VarExp. 'x))))
  (is (= '(lambda (x) (f (f x)))
         (unparse-expression
           (LambdaExp. 'x (AppExp. (VarExp. 'f) (AppExp. (VarExp. 'f) (VarExp. 'x))))))))

;; parse-expression
(deftest test-parse-expression
  (is (= (VarExp. 'x) (parse-expression 'x)))
  (is (= (LambdaExp. 'x (AppExp. (VarExp. 'f) (AppExp. (VarExp. 'f) (VarExp. 'x))))
         (parse-expression '(lambda (x) (f (f x))))))
  (is (thrown? Exception (parse-expression nil))))
