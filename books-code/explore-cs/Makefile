
#_______________________________________________________________________________
#                                                                 Run All Tests

test:
	@ mit-scheme --compiler -heap 6000 --batch-mode --no-init-file --eval '(set! load/suppress-loading-message? #t)' --eval '(begin (load "load") (load "test/load") (run-tests-and-exit))'

#_______________________________________________________________________________
#                                                                      Clean Up

clean:
	rm *.bin *.bci *.com *.ext

.PHONY: test clean
