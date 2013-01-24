(ns eopl.chap4.sec4x-env
  (:use eopl.chap4.sec4x-grammar)
  (:import (eopl.chap4.sec4x_grammar Closure ARef)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment
(defrecord EmptyEnvRecord [])
(defrecord ExtendedEnvRecord [syms vals env])

(declare apply-env apply-env-ref list-find-position list-index rib-find-position)
(defn empty-env []
  (EmptyEnvRecord.))
(defn extend-env [syms vals env]
  (ExtendedEnvRecord. syms (atom (vec vals)) env))
(defn extend-env-recursively [proc-names idss bodies old-env]
  (let [len (count proc-names)
        vec (atom [])
        env (ExtendedEnvRecord.
              proc-names vec old-env)]
    (do
      (doseq [pos (range len)]
        (swap! vec conj (Closure.
                          (nth idss pos)
                          (nth bodies pos)
                          env)))
      env)))
(defn apply-env [env sym]
  (deref_ (apply-env-ref env sym)))
(defn apply-env-ref [env sym]
  (condp instance? env
    EmptyEnvRecord (throw (Exception. (str 'apply-env-ref ": No binding for " sym)))
    ExtendedEnvRecord (let [pos (rib-find-position sym (:syms env))]
                        (if (number? pos)
                          (ARef. pos (:vals env))
                          (apply-env-ref (:env env) sym)))))
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
(def rib-find-position list-find-position)
