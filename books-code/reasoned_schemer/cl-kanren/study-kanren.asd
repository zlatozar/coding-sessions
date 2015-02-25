;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2008, Matthew Swank
;;; All rights reserved.

(asdf:defsystem :study-kanren
  :serial t
  :depends-on (#:alexandria)

  :components
  ((:file "packages")
   (:file "kanren")
   (:file "kanren-lib")
   (:file "kanren-basic-queries")
   (:file "kanren-tests")))
