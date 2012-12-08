(ns eopl.chap1.sec13-test
  (:use clojure.test
        eopl.chap1.sec13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3.1 Free and Bound Variables
;; occurs-free?
(deftest test-occurs-free?
  (is (= false (occurs-free? 'x '((lambda (x) x) y))))
  (is (= true (occurs-free? 'y '((lambda (x) x) y)))))

;; occurs-bound?
(deftest test-occurs-bound?
  (is (= true (occurs-bound? 'x '((lambda (x) x) y))))
  (is (= false (occurs-bound? 'y '((lambda (x) x) y)))))
