(ns eopl.chap4.sec4x-tenv
  (:use clojure.pprint
        eopl.chap4.sec4x-grammar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type environment
(defrecord EmptyTenvRecord [])
(defrecord ExtendedTenvRecord [syms vals tenv])
(defrecord TypedefRecord [name definition tenv])

(declare list-find-position list-index)
(defn empty-tenv []
  (EmptyTenvRecord.))
(defn extend-tenv [syms vals tenv]
  (ExtendedTenvRecord. syms vals tenv))
(defn extend-tenv-with-typedef [name definition tenv]
  (TypedefRecord. name definition tenv))

(defn extend-tenv-with-typedef-exp [typename texp tenv]
  (extend-tenv-with-typedef
    typename
    (expand-type-expression texp tenv)
    tenv))
(defn extend-tenv-with-type-exps [ids texps tenv]
  (extend-tenv
    ids
    (expand-type-expressions texps tenv)
    tenv))
(defn apply-tenv [tenv sym]
  (condp instance? tenv
    EmptyTenvRecord (throw (Exception.
                             (str 'apply-tenv
                                  ": Variable " sym " unbound in type environment")))
    ExtendedTenvRecord (let [pos (list-find-position sym (:syms tenv))]
                         (if (number? pos)
                           (nth (:vals tenv) pos)
                           (apply-tenv (:tenv tenv) sym)))
    TypedefRecord (apply-tenv (:tenv tenv) sym)))
(defn- list-find-position [sym los]
  (list-index (fn [sym1] (= sym1 sym)) los))
(defn- list-index [pred ls]
  (cond
    (empty? ls) false
    (pred (first ls)) 0
    :else (let [list-index-r (list-index pred (rest ls))]
            (if (number? list-index-r)
              (+ list-index-r 1)
              false))))

(defn find-typedef [tenv id]
  (condp instance? tenv
    EmptyTenvRecord (throw (Exception.
                             (str 'find-typedef
                                  ": Type id " id " unbound in type environment")))
    ExtendedTenvRecord (find-typedef (:tenv tenv) id)
    TypedefRecord (if (= (:name tenv) id)
                    (:definition tenv)
                    (find-typedef (:tenv tenv) id))))
