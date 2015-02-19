;; Needed by Guile 2.0

(define (remp p ls)
  (cond ((null? ls) '())
        ((p (car ls)) (remp p (cdr ls)))
        (else (cons (car ls) (remp p (cdr ls))))))

(define (exists p ls)
  (cond ((null? ls) #f)
        ((p (car ls)) #t)
        (else (exists p (cdr ls)))))

(define sorter
  (lambda (ls) (sort ls lex<=?)))

(define datum->string
  (lambda (x)
    (with-output-to-string (lambda () (display x)))))
