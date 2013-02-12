(ns eopl.chap3.sec31_7-callbyval-env
  (:use eopl.chap3.sec31_7-callbyval-grammar)
  (:import (eopl.chap3.sec31_7_callbyval_grammar Closure ARef)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment
(defrecord EmptyEnvRecord [])
(defrecord ExtendedEnvRecord [syms vals env])
#_(defrecord RecursivelyExtendedEnvRecord [proc-names idss bodies env])

(declare apply-env apply-env-ref list-find-position list-index rib-find-position)
(defn empty-env []
  (EmptyEnvRecord.))
(defn extend-env [syms vals env]
  (ExtendedEnvRecord. syms (atom (vec vals)) env))
;; Figure 3.9
#_(defn extend-env-recursively [proc-names idss bodies old-env]
  (let [rec-env (atom nil)]
    (do
      (reset! rec-env
              (fn [sym]
                (let [pos (list-find-position sym proc-names)]
                  (if (number? pos)
                    (Closure.
                      (nth idss pos)
                      (nth bodies pos)
                      @rec-env)
                    (apply-env old-env sym)))))
      @rec-env)))
;; Figure 3.10
#_(defn extend-env-recursively [proc-names idss bodies old-env]
  (RecursivelyExtendedEnvRecord.
    proc-names idss bodies old-env))
;; Figure 3.12
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
