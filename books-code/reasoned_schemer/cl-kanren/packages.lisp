;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2008, Matthew Swank
;;; All rights reserved.

(defpackage :study-kanren
  (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)

  (:export #:unify
           #:walk*
           #:reify-subst
           ;; extendable generics
           #:equivp
           #:unify-impl
           #:walk-impl
           #:reify-subst-impl

;;; user-interface
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

;;; lib-functions
           #:choice-case
           #:map-choice
           #:make-nary-relation
           #:permute-binary-relation
           #:make-binary-relation
           #:permute-ternary-relation
           #:make-ternary-relation))

(defpackage #:pcl-test
  (:documentation "Use defined in the book 'Practical Common Lisp' test framework
to test chapter exercises.")
  (:use #:common-lisp)
  (:export #:deftest
           #:check))

(defpackage :kanren-test
  (:use :cl
        :study-kanren
        :pcl-test)
  (:export #:ch1-tests
           #:ch2-tests
           #:ch3-tests
           #:ch6-tests
           #:ch10-tests))

;; Contains "The Reasoned Schemere" book code
(defpackage :book
  (:use :cl :study-kanren))
