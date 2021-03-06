(ns eopl.chap4.sec4x-type-test
  (:use clojure.test
        eopl.chap4.sec4x-grammar
        eopl.chap4.sec4x-env
        eopl.chap4.sec4x-interp
        eopl.chap4.sec4x-parser
        eopl.chap4.sec4x-type)
  (:import (eopl.chap4.sec4x_grammar
             LitExp VarExp PrimappExp IfExp LetExp ProcExp AppExp LetrecExp VarassignExp BeginExp
             AddPrim SubtractPrim MultPrim IncrPrim DecrPrim ZeroTestPrim
             AtomicType ProcType)))

(deftest test-type-to-external-form
  (is (= '(int * (int -> bool) -> int)
         (type-to-external-form
           (ProcType.
             (list int-type (ProcType.
                              (list int-type)
                              bool-type))
             int-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Checker
(deftest test-fresh-type
  (is (= (AtomicType. 'xx8) (fresh-type 'xx)))
  (is (= (AtomicType. 'xx9) (fresh-type 'xx)))
  (is (= (AtomicType. 'xx10) (fresh-type 'xx))))

(deftest test-type-check
  (is (= '(int -> int)
         (type-check "proc (int x) add1(x)")))
  (is (= '(int -> int)
         (type-check "
letrec
  int fact (int x) =
        if zero?(x) then 1 else *(x,(fact sub1(x)))
in fact")))
  (is (= 'myint1
         (type-check "lettype myint = int myint zero () = 1 in (zero)")))
  (is (= 'myint2
         (type-check "
lettype myint = int
  myint zero () = 1
  myint succ (myint x) = add1(x)
  myint pred (myint x) = sub1(x)
  bool  iszero? (myint x) = zero?(-(x,1))
in (succ (zero))")))
  (is (thrown? Exception
               (type-check "
lettype myint = int
  myint zero () = 1
  myint succ (myint x) = add1(x)
  myint pred (myint x) = sub1(x)
  bool  iszero? (myint x) = zero?(-(x,1))
in add1((zero))")))
  (is (= 'myint4
         (type-check "
lettype myint = int
  myint zero () = 1
  myint succ (myint x) = add1(x)
  myint pred (myint x) = sub1(x)
  bool  iszero? (myint x) = zero?(-(x,1))
in (succ (zero))")))
  (is (thrown? Exception
               (type-check "
lettype myint = int
  myint zero () = 1
  myint succ (myint x) = add1(x)
  myint pred (myint x) = sub1(x)
  bool  iszero? (myint x) = zero?(-(x,1))
in add1((zero))")))
  (is (= 'int
         (type-check "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k))
                     then val
                     else (apply-ff old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
   in (apply-ff ff1 2)")))
  (is (thrown? Exception
               (type-check "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k))
                     then val
                     else (apply-ff old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
   in (ff1 2)")))
  )
