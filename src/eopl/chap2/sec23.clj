(ns eopl.chap2.sec23)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.3.2 Procedural Representation
;; environment
(declare apply-env list-find-position list-index)
(defn empty-env []
  (fn [sym]
    (throw (Exception. (str 'apply-env ": No binding for " sym)))))
(defn extend-env [syms vals env]
  (fn [sym]
    (let [pos (list-find-position sym syms)]
      (if (number? pos)
        (nth vals pos)
        (apply-env env sym)))))
(defn apply-env [env sym]
  (env sym))
(defn list-find-position [sym los]
  (list-index (fn [sym1] (= sym1 sym)) los))
(defn list-index [pred ls]
  (cond
    (nil? ls) false
    (pred (first ls)) 0
    :else (let [list-index-r (list-index pred (rest ls))]
            (if (number? list-index-r)
              (+ list-index-r 1)
              false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.3.3 Abstract Syntax Tree Representation
;; environment
(defrecord EmptyEnvRecord [])
(defn empty-env-record [] (EmptyEnvRecord.))
(defrecord ExtendedEnvRecord [syms vals env])
(defn extended-env-record [syms vals env] (ExtendedEnvRecord. syms vals env))

(defn empty-env1 []
  (empty-env-record))
(defn extend-env1 [syms vals env]
  (extended-env-record syms vals env))
(defn apply-env1 [env sym]
  (condp instance? env
    EmptyEnvRecord (throw (Exception. (str 'apply-env1 ": No binding for " sym)))
    ExtendedEnvRecord (let [pos (list-find-position sym (:syms env))]
                        (if (number? pos)
                          (nth (:vals env) pos)
                          (apply-env1 env sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.3.4 Alternative Data Structure Representations
;; environment
(declare rib-find-position)
(defn empty-env2 []
  '())
(defn extend-env2 [syms vals env]
  (cons (list syms vals) env))
(defn apply-env2 [env sym]
  (if (empty? env)
    (throw (Exception. (str 'apply-env2 ": No binding for " sym)))
    (let [syms (first (first env))
          vals (second (first env))
          env (rest env)]
      (let [pos (rib-find-position sym syms)]
        (if (number? pos)
          (nth vals pos)
          (apply-env2 env sym))))))
(def rib-find-position list-find-position)
