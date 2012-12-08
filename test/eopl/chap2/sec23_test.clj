(ns eopl.chap2.sec23-test
  (:use clojure.test
        eopl.chap2.sec23))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.3.2 Procedural Representation
;; environment
(deftest test-env
  (is (thrown? Exception (apply-env (empty-env) 'x)))
  (is (= 6 (apply-env (extend-env '(d x y) '(6 7 8) (empty-env)) 'd)))
  (is (= 7 (apply-env (extend-env '(d x y) '(6 7 8) (empty-env)) 'x)))
  (is (= 8 (apply-env (extend-env '(d x y) '(6 7 8) (empty-env)) 'y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.3.3 Abstract Syntax Tree Representation
;; environment
(deftest test-env1
  (is (thrown? Exception (apply-env1 (empty-env1) 'x)))
  (is (= 6 (apply-env1 (extend-env1 '(d x y) '(6 7 8) (empty-env1)) 'd)))
  (is (= 7 (apply-env1 (extend-env1 '(d x y) '(6 7 8) (empty-env1)) 'x)))
  (is (= 8 (apply-env1 (extend-env1 '(d x y) '(6 7 8) (empty-env1)) 'y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.3.4 Alternative Data Structure Representations
;; environment
(deftest test-env2
  (is (thrown? Exception (apply-env2 (empty-env2) 'x)))
  (is (= 6 (apply-env2 (extend-env2 '(d x y) '(6 7 8) (empty-env2)) 'd)))
  (is (= 7 (apply-env2 (extend-env2 '(d x y) '(6 7 8) (empty-env2)) 'x)))
  (is (= 8 (apply-env2 (extend-env2 '(d x y) '(6 7 8) (empty-env2)) 'y))))
