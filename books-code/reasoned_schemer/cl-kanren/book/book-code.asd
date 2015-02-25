;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2008, Matthew Swank
;;; All rights reserved.

(asdf:defsystem :book
  :components
  ((:file "packages")
   (:file "book-code" :depends-on ("packages"))) :depends-on (:study-kanren))
