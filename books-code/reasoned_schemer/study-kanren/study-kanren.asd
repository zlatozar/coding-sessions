;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2008, Matthew Swank
;;; All rights reserved.

(asdf:defsystem :study-kanren
  :serial t
  :depends-on (#:alexandria)

  :components

;;; miniKanren implementation
  ((:file "packages")
   (:file "impl/mini-kanren" :depends-on ("packages"))
   (:file "impl/kanren-lib")
   (:file "kanren-basic-queries")

   (:file "tools/test" :depends-on ("packages"))
   (:file "impl/test-mini-kanren" :depends-on ("tools/test" "impl/mini-kanren"))

   (:file "book-code")))
