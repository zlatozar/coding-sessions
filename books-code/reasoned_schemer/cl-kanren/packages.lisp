;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2008, Matthew Swank
;;; All rights reserved.

(defpackage :study-kanren
  (:use :cl)
  (:import-from #:alexandria #:with-gensyms)

  (:export
;;; developer-interface
   #:unify #:walk* #:reify-subst
   ;; extendable generics
   #:equivp #:unify-impl
   #:walk-impl #:reify-subst-impl

;;; user-interface
   #:else
   #:+succeed+
   #:+fail+ #:jog #:run #:run* #:==
   #:fresh #:conde #:condi #:condu
   #:all #:alli #:conda

;;; basic queries
   #:nullo #:conso #:caro #:cdro #:pairo #:eq-caro
   #:listo #:membero #:appendo
   #:brancho #:flatteno

;;; lib-functions
   #:choice-case #:map-choice #:make-nary-relation
   #:permute-binary-relation #:make-binary-relation
   #:permute-ternary-relation #:make-ternary-relation))

(defpackage :kanren-test
  (:use :cl)
  (:export #:run-tests))
