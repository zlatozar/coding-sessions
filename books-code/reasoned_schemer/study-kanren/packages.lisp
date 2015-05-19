;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2008, Matthew Swank
;;; All rights reserved.

(defpackage :mini-kanren
  (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)

  (:export #:unify
           #:walk*
           #:reify-subst
           #:equivp
           #:unify-impl
           #:walk-impl
           #:reify-subst-impl

           ;; user-interface
           #:else
           #:+succeed+
           #:+fail+
           #:jog
           #:run
           #:run*
           #:==
           #:fresh
           #:conde
           #:condi
           #:condu
           #:all
           #:alli
           #:conda

;;; basic queries
           #:nullo
           #:conso
           #:caro
           #:cdro
           #:pairo
           #:eq-caro
           #:listo
           #:membero
           #:appendo
           #:brancho
           #:flatteno

           ;; lib-functions
           #:choice-case
           #:map-choice
           #:make-nary-relation
           #:permute-binary-relation
           #:make-binary-relation
           #:permute-ternary-relation
           #:make-ternary-relation))

(defpackage #:pcl-test
  (:documentation "Use defined in the book 'Practical Common Lisp' test framework
to test mini-kanren implementation.")
  (:use :cl)
  (:export #:deftest
           #:check))

;; Contains "The Reasoned Schemer" book code
(defpackage :book
  (:use :cl
        :pcl-test
        :mini-kanren)
  ;; Tests
  (:export #:ch1-tests
           #:ch2-tests
           #:ch3-tests
           #:ch6-tests
           #:ch10-tests))

;;; ____________________________________________________________________________
;;;                                                                      Public

(defpackage :cl-kanren
  (:documentation "Expose functions defined in 'The Reasoned Schemer' book")
  (:use :cl))
