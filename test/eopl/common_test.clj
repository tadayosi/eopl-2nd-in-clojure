(ns eopl.common-test
  (:use clojure.test
        eopl.common))

(define-datatype Abc abc
  [a b c])
(deftest test-define-datatype
  (is (= [1 2 3] (let [x (Abc. 1 2 3)]
                   [(:a x) (:b x) (:c x)])))
  
  (is (= [1 2 3] (let [x (abc 1 2 3)]
                   [(:a x) (:b x) (:c x)]))))