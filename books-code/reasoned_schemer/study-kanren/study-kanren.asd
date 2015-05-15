;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2008, Matthew Swank
;;; All rights reserved.

(asdf:defsystem :study-kanren
  :depends-on (#:alexandria)

  :serial t
  :components  ((:file "packages")

                ;; miniKanren
                (:file "impl/mini-kanren" :depends-on ("packages"))
                (:file "impl/kanren-lib" :depends-on ("impl/mini-kanren"))
                (:file "kanren-basic-queries")

                ;; Test miniKanren implementation
                (:file "tools/test" :depends-on ("packages"))
                (:file "impl/test-mini-kanren" :depends-on ("tools/test" "impl/mini-kanren"))

                ;; Functions defined in book
                (:file "book-code" :depends-on ("impl/mini-kanren"))))
