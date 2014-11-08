;; Helper functions
(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(define (assert-same-dictionary-lists expected got)
  (assert-equal (length expected) (length got))
  (assert-true (every dict:equal? expected got)))

(load-relative "../test-manager/load")

;; Project tests
(load-relative "test-manager-test")
(load-relative "programming-the-computer-test")
(load-relative "repetition-through-recursion-test")
