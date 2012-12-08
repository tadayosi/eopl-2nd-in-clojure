(ns eopl.chap2.sec21-test
  (:use clojure.test
        eopl.chap2.sec21))

;; plus
(def one (succ zero))
(def two (succ one))
(def three (succ two))
(deftest test-plus
  (is (= one (plus one zero)))
  (is (= two (plus one one)))
  (is (= three (plus two one))))
