(ns eopl.chap3.sec31_7-callbyval-test2
  (:use clojure.test
        eopl.chap3.sec31_7-callbyval-parser2))

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
  (is (= 2 (run "(proc (x) add1(x) 1)")))
  (is (= 25 (run "
let f = proc (y, z) +(y,-(z,5))
in (f 2 28)")))
  (is (= 25 (run "
let x = 5
in let f = proc (y, z) +(y,-(z,x))
       x = 28
   in (f 2 x)")))
  (is (= 176 (run "
let x = 5
in let x = 38
       f = proc (y, z) *(y,+(x,z))
       g = proc (u) +(u,x)
   in (f (g 3) 17)")))
  (is (= 1 (run "zero?(0)")))
  (is (= 0 (run "zero?(sub1(5))")))
  (is (= 720 (run "
letrec
  fact(x) = if zero?(x) then  1 else *(x, (fact sub1(x)))
in (fact 6)")))
  (is (= 1 (run "
letrec
  even(x) = if zero?(x) then 1 else (odd sub1(x))
  odd(x)  = if zero?(x) then 0 else (even sub1(x))
in (odd 13)")))
  (is (= 1 (run "
let x = 0
in letrec
     even() = if zero?(x)
              then 1
              else let d = set x = sub1(x)
                   in (odd)
     odd()  = if zero?(x)
              then 0
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
in let p = proc (x) let d = set x = add1(x)
                    in x
   in +((p x),(p x))")))
  (is (= 1 (run "begin 1 end")))
  (is (= 3 (run "begin 1; 2; +(1,2) end")))
  (is (= -1 (run "
let a = 3
    b = 4
    swap = proc (x,y)
             let temp = x
             in begin
                  set x = y;
                  set y = temp
                end
in begin
     (swap a b);
     -(a,b)
end")))
  )
