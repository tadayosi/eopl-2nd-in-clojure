(ns eopl.chap1.sec12-test
  (:use clojure.test
        eopl.chap1.sec12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2.2 Some Important Examples
;; remove-first
(deftest test-remove-first
  (is (= '(c b)
         (remove-first 'a '(c a b)))))

;; remove
(deftest test-remove_
  (is (= '(c b c)
         (remove_ 'a '(c a b a c)))))

;; subst
(deftest test-subst
  (is (= '((a c) (a () d))
         (subst 'a 'b '((b c) (b () d))))))

;; notate-depth
(deftest test-notate-depth
  (is (= '((a 0) ((b 1) () (c 1)) (((d 2))) (e 0))
         (notate-depth '(a (b () c) ((d)) e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2.3 Other Patterns of Recursion
;; list-sum
(deftest test-list-sum
  (is (= 15 (list-sum '(1 2 3 4 5))))
  (is (= 0 (list-sum ()))))

;; vector-sum
(deftest test-vector-sum
  (is (= 15 (vector-sum [1 2 3 4 5])))
  (is (= 0 (vector-sum []))))
