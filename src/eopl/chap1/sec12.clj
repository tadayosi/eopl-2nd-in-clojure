(ns eopl.chap1.sec12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2.2 Some Important Examples
;; remove-first
(defn remove-first [s los]
  (if (empty? los)
    '()
    (if (= (first los) s)
      (rest los)
      (cons (first los) (remove-first s (rest los))))))

;; remove
(defn remove_ [s los]
  (if (empty? los)
    '()
    (if (= (first los) s)
      (remove_ s (rest los))
      (cons (first los) (remove_ s (rest los))))))

;; subst
(declare subst-in-symbol-expression)
(defn subst [new old slist]
  (if (empty? slist)
    '()
    (cons
      (subst-in-symbol-expression new old (first slist))
      (subst new old (rest slist)))))
(defn subst-in-symbol-expression [new old se]
  (if (symbol? se)
    (if (= se old) new se)
    (subst new old se)))

;; notate-depth
(declare notate-depth-in-s-list)
(declare notate-depth-in-symbol-expression)
(defn notate-depth [slist]
  (notate-depth-in-s-list slist 0))
(defn notate-depth-in-s-list [slist d]
  (if (empty? slist)
    '()
    (cons
      (notate-depth-in-symbol-expression (first slist) d)
      (notate-depth-in-s-list (rest slist) d))))
(defn notate-depth-in-symbol-expression [se d]
  (if (symbol? se)
    (list se d)
    (notate-depth-in-s-list se (+ d 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2.3 Other Patterns of Recursion
;; list-sum
(defn list-sum [lon]
  (if (empty? lon)
    0
    (+ (first lon)
       (list-sum (rest lon)))))

;; vector-sum
(defn partial-vector-sum [von n]
  (if (zero? n)
    0
    (+ (von (- n 1))
       (partial-vector-sum von (- n 1)))))
(defn vector-sum [von]
  (partial-vector-sum von (count von)))
