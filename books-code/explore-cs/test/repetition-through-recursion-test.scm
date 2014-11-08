(load-relative "../test-manager/load")

;; First is band rest are the albums
(define rs-albums
  '(( (Rolling Stones)
      (Black and Blue)
      (Its Only Rock and Roll) )
    ))

(in-test-group
 repetition-through-recursion

 (define-test (inner-loop-test)
   (assert-equal '((Black and Blue) (Its Only Rock and Roll))
                 (CDs-within-category-inner rs-albums '(Rolling Stones)))) )

(run-registered-tests)
