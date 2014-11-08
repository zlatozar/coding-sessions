(load-relative "../test-manager/load")

(in-test-group 
programming-the-computer

 (define-each-test
   (assert-= 8 (new-double)  "Just test if test framework is set up correctly")) )

(run-registered-tests)
