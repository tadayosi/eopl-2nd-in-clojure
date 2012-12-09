(ns eopl.chap2.sec22
  (:use eopl.common))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.1 define-datatype and cases
;; bintree
(define-datatype LeafNode leaf-node
  [datum])
(define-datatype InteriorNode interior-node
  [key left right])

(defn leaf-sum [tree]
  (condp instance? tree
    LeafNode (:datum tree)
    InteriorNode (+ (leaf-sum (:left tree)) (leaf-sum (:right tree)))))

;; s-list
(define-datatype EmptySList empty-s-list
  [])
(define-datatype NonEmptySList non-empty-s-list
  [first rest])
(define-datatype SymbolExp symbol-exp
  [data])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.2 Abstract Syntax and its Representation
;; expression
(define-datatype VarExp var-exp
  [id])
(define-datatype LambdaExp lambda-exp
  [id body])
(define-datatype AppExp app-exp
  [rator rand])

;; occurs-free?
(defn occurs-free? [var exp]
  (condp instance? exp
    VarExp (= (:id exp) var)
    LambdaExp (and (not (= (:id exp) var))
                   (occurs-free? var (:body exp))
    AppExp (or (occurs-free? var (:rator exp))
               (occurs-free? var (:rand exp))))))

;; unparse-expression
(defn unparse-expression [exp]
  (condp instance? exp
    VarExp (:id exp)
    LambdaExp (list 'lambda (list (:id exp))
                    (unparse-expression (:body exp)))
    AppExp (list (unparse-expression (:rator exp))
                 (unparse-expression (:rand exp)))))

;; parse-expression
(defn parse-expression [datum]
  (cond
    (symbol? datum) (VarExp. datum)
    (list? datum) (if (= (first datum) 'lambda)
                    (lambda-exp (first (second datum))
                                (parse-expression (nth datum 2)))
                    (app-exp
                      (parse-expression (first datum))
                      (parse-expression (second datum))))
    :else (throw (Exception. (str 'parse-expression
                                  ": Invalid concrete syntax " datum)))))
