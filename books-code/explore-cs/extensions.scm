;; Library files for Exploring Computer Science with Scheme
;; Oliver Grillmeyer
;; Version 1.5, 10/7/97

;; Add the following code if error does not exist in your version of Scheme
;; (define error-setup 'init)
;;
;; (call-with-current-continuation
;;   (lambda (stop)
;;     (set! error-setup stop)))
;;
;; Print an error message made up of the arguments to the function.
;; (define error
;;   (let ( (newline newline) (display display) (car car) (cdr cdr)
;;          (for-each for-each) (error-setup error-setup) )
;;     (lambda vals
;;       (newline)
;;       (display "Error: ")
;;       (display (car vals))
;;       (for-each (lambda (val) (display " ") (display val)) (cdr vals))
;;       (error-setup '.) )) )

;; (module extentions (
;;                     truncate              ; redefined to give exact number
;;                     round                 ; redefined to give exact number
;;                     first second third fourth fifth rest
;;                     subseq
;;                     position remove count ; all use equal? for comparison
;;                     atom?
;;                     find-if find-if-not count-if count-if-not remove-if keep-if
;;                     rassoc                ; uses equal? for comparison
;;                     every any
;;                     accumulate
;;                     intersection union set-difference subset? adjoin
;;                     random init-random)

;; Redefine truncate to return an exact integer.
(set! truncate
  (let ( (truncate truncate) (inexact->exact inexact->exact) )
    (lambda (number)
      (inexact->exact (truncate number)) )) )

;; Redefine round to return an exact integer.
(set! round
  (let ( (round round) (inexact->exact inexact->exact) )
    (lambda (number)
      (inexact->exact (round number)) )) )

;; Return the first element of a list.
(define first car)

;; Return the second element of a list.
(define second cadr)

;; Return the third element of a list.
(define third caddr)

;; Return the fourth element of a list.
(define fourth cadddr)

;; Return the fifth element of a list.
(define fifth
  (let ( (car car) (cddddr cddddr) )
    (lambda (lst)
      (car (cddddr lst)) )) )

;; Return the rest of a list.
(define rest cdr)

;; Return lst without last num elements.
(define list-head
  (let ( (>= >=) (length length) (= =) (cons cons) (car car)
         (cdr cdr) )
    (lambda (lst num)
      (cond ((>= num (length lst)) '())
            ((= num 0) lst)
            (else (cons (car lst) (list-head (cdr lst) num)))) )) )

;; Return subsection of lst from positions start to end-1.
(define subseq
  (let ( (length length) (null? null?) (not not) (<= <=)
         (error error) (list-head list-head) (list-tail list-tail) )
    (lambda (lst start . args)
      (let* ( (len (length lst))
              (end (if (null? args) len (car args))) )
        (cond ((not (<= 0 start len))
               (error "Improper start value for subseq:" start))
              ((not (<= 0 start end len))
               (error "Improper end value for subseq:" end))
              (else
               (list-head (list-tail lst start) (- len end))))) )) )

;; Return the position (base 0) of the first occurrence of elt in lst.
(define position-helper
  (let ( (null? null?)
         (equal? equal?)
         (car car)
         (cdr cdr)
         (+ +) )
    (lambda (elt lst num)
      (cond ((null? lst) #f)
            ((equal? elt (car lst)) num)
            (else (position-helper elt (cdr lst) (+ num 1)))) )) )

(define position
  (let ( (position-helper position-helper) )
    (lambda (elt lst)
      (position-helper elt lst 0) )) )

;; Return lst with all occurrences of elt removed.
(define remove
  (let ( (null? null?) (equal? equal?) (car car) (cdr cdr)
         (cons cons) )
    (lambda (elt lst)
      (cond ((null? lst) '())
            ((equal? elt (car lst)) (remove elt (cdr lst)))
            (else (cons (car lst) (remove elt (cdr lst))))) )) )

;; Return the number of times elt occurs in lst.
(define count
  (let ( (null? null?) (equal? equal?) (car car) (cdr cdr) (+ +) )
    (lambda (elt lst)
      (cond ((null? lst) 0)
            ((equal? elt (car lst))  (+ 1 (count elt (cdr lst))))
            (else (count elt (cdr lst)))) )) )

;; Return #t if item is a symbol or a number, #f otherwise.
(define atom?
  (let ( (symbol? symbol?) (number? number?) )
    (lambda (item)
      (or (symbol? item) (number? item)) )) )

;; Return the first element in lst that satisfies func, or #f if no
;; elements satisfy func.
(define find-if
  (let ( (null? null?) (car car) (cdr cdr) )
    (lambda (func lst)
      (cond ((null? lst) #f)
            ((func (car lst)) (car lst))
            (else (find-if func (cdr lst)))) )) )

;; Return the first element in lst that does not satisfy func, or #f
;; if all elements satisfy func.
(define find-if-not
  (let ( (null? null?) (not not) (car car) (cdr cdr) )
    (lambda (func lst)
      (cond ((null? lst) #f)
            ((not (func (car lst))) (car lst))
            (else (find-if-not func (cdr lst)))) )) )

;; Return the number of elements in lst that satisfy func.
(define count-if
  (let ( (null? null?) (car car) (cdr cdr) (+ +) )
    (lambda (func lst)
      (cond ((null? lst) 0)
            ((func (car lst)) (+ 1 (count-if func (cdr lst))))
            (else (count-if func (cdr lst)))) )) )

;; Return the number of elements in lst that do not satisfy func.
(define count-if-not
  (let ( (null? null?) (not not) (car car) (cdr cdr) (+ +) )
    (lambda (func lst)
      (cond ((null? lst) 0)
            ((not (func (car lst)))
             (+ 1 (count-if-not func (cdr lst))))
            (else (count-if-not func (cdr lst)))) )) )

;; Return lst with all elements satisfying func removed.
(define remove-if
  (let ( (null? null?) (car car) (cdr cdr) (cons cons) )
    (lambda (func lst)
      (cond ((null? lst) '())
            ((func (car lst))
             (remove-if func (cdr lst)))
            (else
             (cons (car lst) (remove-if func (cdr lst))))) )) )

;; Return lst with all elements satisfying func.
(define keep-if
  (let ( (null? null?) (not not) (car car) (cdr cdr) (cons cons) )
    (lambda (func lst)
      (cond ((null? lst) '())
            ((not (func (car lst)))
             (keep-if func (cdr lst)))
            (else
             (cons (car lst) (keep-if func (cdr lst))))) )) )

;; Like assoc but return the first pair whose cdr matches elt.
(define rassoc
  (let ( (find-if find-if) (equal? equal?) (cdr cdr) )
    (lambda (elt assoc-list)
      (find-if (lambda (dotted-pair)
                 (equal? (cdr dotted-pair) elt) )
               assoc-list) )) )

;; every and any each take a variable number of lists as arguments
;; and apply the function to those N lists using apply and map.
;; To make the recursive call, apply is used to convert a list of
;; argument lists into separate arguments.

;; Return final true return value if all successive elements in lists
;; satisfy func, #f otherwise.
(define every
  (let ( (null? null?) (car car) (cdr cdr) (apply apply) (map map)
         (cons cons) (member member) )
    (lambda (func . lists)
      (cond ((member #t (map null? lists)) #t)
            ((member #t (map (lambda (lst) (null? (cdr lst))) lists))
             (apply func (map car lists)))
            (else
             (and (apply func (map car lists))
                  (apply every (cons func (map cdr lists)))))) )) )

;; Return the first true value from applying func to successive
;; elements in lists, or #f if no elements satisfy func.
(define any
  (let ( (null? null?) (car car) (cdr cdr) (apply apply) (map map)
         (cons cons) (member member) )
    (lambda (func . lists)
      (if (member #t (map null? lists))
          #f
          (or (apply func (map first lists))
              (apply any (cons func (map rest lists))))) )) )

;; Return result of applying func to elements of lst in the following
;; manner: func is applied to the first two elements of lst then to
;; that result and the third element, then to that result and the
;; fourth element, and so on until all elements have been applied.
(define accum-tail
  (let ( (null? null?) (car car) (cdr cdr) )
    (lambda (func lst answer)
      (if (null? lst)
          answer
          (accum-tail func (cdr lst) (func answer (car lst)))) )) )

(define accumulate
  (let ( (null? null?) (car car) (cdr cdr) (accum-tail accum-tail) )
    (lambda (func lst)
      (if (null? lst)
          (func)
          (accum-tail func (cdr lst) (car lst))) )) )

;; Return the elements that set1 and set2 have in common.
(define intersection
  (let ( (null? null?) (member member) (car car) (cdr cdr)
         (cons cons) )
    (lambda (set1 set2)
      (cond ((or (null? set1) (null? set2))
             '())
            ((member (car set1) set2)
             (cons (car set1) (intersection (cdr set1) set2)))
            (else
             (intersection (cdr set1) set2))) )) )

;; Return the elements that exist in either set1 or set2.
(define union
  (let ( (null? null?) (member member) (car car) (cdr cdr)
         (cons cons) )
    (lambda (set1 set2)
      (cond ((null? set1)
             set2)
            ((member (car set1) set2)
             (union (cdr set1) set2))
            (else
             (cons (car set1) (union (cdr set1) set2)))) )) )

;; Return the elements that exist in set1 but not in set2.
(define set-difference
  (let ( (null? null?) (member member) (car car) (cdr cdr)
         (cons cons) )
    (lambda (set1 set2)
      (cond ((null? set2)
             set1)
            ((null? set1)
             '())
            ((member (car set1) set2)
             (set-difference (cdr set1) set2))
            (else
             (cons (car set1)
                   (set-difference (cdr set1) set2)))) )) )

;; Return #t if all the elements in set1 exist in set2.
(define subset?
  (let ( (null? null?) (member member) (car car) (cdr cdr) )
    (lambda (set1 set2)
      (cond ((null? set1)
             #t)
            ((null? set2)
             #f)
            (else
             (and (member (car set1) set2)
                  (subset? (cdr set1) set2)))) )) )

;; Return a new set of item and the elements in set if item does not
;; exist in set, otherwise return set.
(define adjoin
  (let ( (member member) (cons cons) )
    (lambda (item set)
      (if (member item set)
          set
          (cons item set)) )) )

;; The following code is a modification of a random function used by
;; Brian Harvey and Matt Wright in their text "Simply Scheme" which
;; they obtained from an old version of the Scheme Library (SLIB)
;; written by Aubrey Jaffer.

;; random has been modified to allow an initial seed to be created
;; using init-random.
(define random 0)

(define (init-random seed)
  (set! random
    (let ( (*seed* seed) (quotient quotient) (modulo modulo)
           (+ +) (- -) (* *) (> >) )
      (lambda (x)
        (let* ((hi (quotient *seed* 127773))
               (low (modulo *seed* 127773))
               (test (- (* 16807 low) (* 2836 hi))))
          (if (> test 0)
              (set! *seed* test)
              (set! *seed* (+ test 2147483647))))
        (modulo *seed* x) ))) )

(init-random 1)
