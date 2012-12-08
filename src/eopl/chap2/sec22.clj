(ns eopl.chap2.sec22)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.1 define-datatype and cases
;; bintree
(defrecord LeafNode [datum])
(defn leaf-node [datum] (LeafNode. datum))
(defrecord InteriorNode [key left right])
(defn interior-node [key left right] (InteriorNode. key left right))

(defn leaf-sum [tree]
  (condp instance? tree
    LeafNode (:datum tree)
    InteriorNode (+ (leaf-sum (:left tree)) (leaf-sum (:right tree)))))

;; s-list
(defrecord EmptySList [])
(defn empty-s-list [] (EmptySList.))
(defrecord NonEmptySList [first rest])
(defn non-empty-s-list [first rest] (NonEmptySList. first rest))
(defrecord SymbolExp [data])
(defn symbol-exp [data] (SymbolExp. data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.2 Abstract Syntax and its Representation
;; expression
(defrecord VarExp [id])
(defn var-exp [id] (VarExp. id))
(defrecord LambdaExp [id body])
(defn lambda-exp [id body] (LambdaExp. id body))
(defrecord AppExp [rator rand])
(defn app-exp [rator rand] (AppExp. rator rand))

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
