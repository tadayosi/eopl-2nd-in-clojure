(ns eopl.chap1.sec13)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3.1 Free and Bound Variables
;; occurs-free?
(defn occurs-free? [var exp]
  (cond
    (symbol? exp) (= var exp)
    (= (first exp) 'lambda) (and (not (= (first (second exp)) var))
                                 (occurs-free? var (nth exp 2)))
    :else (or (occurs-free? var (first exp))
              (occurs-free? var (second exp)))))

;; occurs-bound?
(defn occurs-bound? [var exp]
  (cond
    (symbol? exp) false
    (= (first exp) 'lambda) (or (occurs-bound? var (nth exp 2))
                                (and (= (first (second exp)) var)
                                     (occurs-free? var (nth exp 2))))
    :else (or (occurs-bound? var (first exp))
              (occurs-bound? var (second exp)))))
