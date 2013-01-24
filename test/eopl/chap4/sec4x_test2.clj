(ns eopl.chap4.sec4x-test2
  (:use clojure.test
        eopl.chap4.sec4x-parser2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
(deftest test-run
  (is (= 5 (run "5")))
  (is (= 3 (run "add1(2)")))
  (is (= 5 (run "+(add1(2),-(6,4))")))
  (is (= 2 (run "if 1 then 2 else 3")))
  (is (= 3 (run "if -(3,+(1,2)) then 2 else 3")))
  (is (= 11 (run "
let x = 5
    y = 6
in +(x,y)")))
  (is (= 4 (run "
let x = 1
in let x = +(x,2)
   in add1(x)")))
  (is (= 2 (run "(proc (int x) add1(x) 1)")))
  (is (= 25 (run "
let f = proc (int y, int z) +(y,-(z,5))
in (f 2 28)")))
  (is (= 25 (run "
let x = 5
in let f = proc (int y, int z) +(y,-(z,x))
       x = 28
   in (f 2 x)")))
  (is (= 176 (run "
let x = 5
in let x = 38
       f = proc (int y, int z) *(y,+(x,z))
       g = proc (int u) +(u,x)
   in (f (g 3) 17)")))
  (is (= 1 (run "zero?(0)")))
  (is (= 0 (run "zero?(sub1(5))")))
  (is (= 720 (run "
letrec
  int fact(int x) = if zero?(x) then  1 else *(x, (fact sub1(x)))
in (fact 6)")))
  (is (= 1 (run "
letrec
  bool even(int x) = if zero?(x) then true else (odd sub1(x))
  bool odd(int x)  = if zero?(x) then false else (even sub1(x))
in (odd 13)")))
  (is (= 1 (run "
let x = 0
in letrec
     bool even() = if zero?(x)
                   then true
                   else let d = set x = sub1(x)
                        in (odd)
     bool odd()  = if zero?(x)
                   then false
                   else let d = set x = sub1(x)
                        in (even)
   in let d = set x = 13
      in (odd)")))
  (is (= 3 (run "
let g = let count = 0
        in proc ()
             let d = set count = add1(count)
             in count
in +((g),(g))")))
  (is (= 202 (run "
let x = 100
in let p = proc (int x) let d = set x = add1(x)
                        in x
   in +((p x),(p x))")))
  (is (= 1 (run "begin 1 end")))
  (is (= 3 (run "begin 1; 2; +(1,2) end")))
  (is (= -1 (run "
let a = 3
    b = 4
    swap = proc (int x, int y)
             let temp = x
             in begin
                  set x = y;
                  set y = temp
                end
in begin
     (swap a b);
     -(a,b)
   end")))
  (is (= 22 (run "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1)
         if zero?(-(k1,k))
         then val
         else (apply-ff old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11
               (extend-ff 2 22
                 (zero-ff)))
   in (apply-ff ff1 2)")))
  (is (= 22 (run "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1)
         if zero?(-(k1,k))
         then val
         else (apply-ff old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11
               (extend-ff 2 22
                 (zero-ff)))
   in (ff1 2)")))
  )
