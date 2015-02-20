#lang cKanren

;; Lecture 1

;; What is == ?

== ; unification operator

;; Check if values are equal and return only 1 result
(run 1 (q)
     (== '(a) '(a b))) ; => () which means 'not equal'

(run 1 (q)
     (== '(a b) '(a b)))

;; How to use 'run'?

;; What should be the value of 'q'?
(run 1 (q)
     (== 5 q))

(run 1 (q)
     (== q q)) ; => (_.0) is the representation of 'q' and '_' tell us that 'q' could be anything

(run 1 (q)
     (== (list 4 q)
         (list q 4))) ; => (4)

;; We could solve it if have more than one variable
(run 1 (q)
     (== (list q 4)
         (list 3 q))) ; => ()

(run 1 (q)
     (== q (list 3))) ; => ((3))

;; occur check
(run 1 (q)
     (== q (list q))) ; => ()

;; Latest versions of miniKanren (Guile fails) supports multiple variables
(run 1 (x y)
     (== (list x 4)
         (list 3 y)))

;; Pass in Guile
(run 1 (q)
     (fresh (x y)   ; not binned logical variables
      (== (list x 4)
          (list 3 y))
      (== (list x y) q))) ; => ((3 4))

;; How to deal with functions

(run 1 (q)
     (== `(lambda (x) x) `(lambda (x) x))) ; => (_.0)

(run 1 (q)
     (== `(lambda (x) x) `(lambda (y) y))) ; => ()

;; == change the meaning of equality
(run 1 (x y)
     (== `(lambda (,x) ,x) `(lambda (,y) ,y))) ; => ((_.0 _.0))
