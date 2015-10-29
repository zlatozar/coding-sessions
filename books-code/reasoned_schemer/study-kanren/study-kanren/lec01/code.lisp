;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STUDY-KANREN; Base: 10 -*-

(in-package :study-kanren)

;; Constants
(defconst +every+ `(:|_.0|))

;;; ____________________________________________________________________________
;;;                                                                   Lecture 1

(deftest lecture1-tests ()
  (check
    (equal (run nil (q)
             (== '(3 ((((4)))) 5) '(3 ((((4)))) 5)))
           +every+)
    ))
