(ns eopl.chap2.sec21)

;; plus
(declare zero iszero? succ pred)
(defn plus [x y]
  (if (iszero? x)
    y
    (succ (plus (pred x) y))))

;; Unary representation
(def zero '())
(def iszero? empty?)
(defn succ [n] (cons true n))
(def pred rest)

;; Scheme number representation
(def zero 0)
(def iszero? zero?)
(defn succ [n] (+ n 1))
(defn pred [n] (- n 1))

;; Bignum representation
