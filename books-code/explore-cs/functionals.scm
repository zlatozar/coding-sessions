;; Code from Chapter 8 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

(load "extensions.scm")

;; Return a list of the square roots of the numbers in a-list.
(define (square-roots-mapping a-list)
  (map sqrt a-list))

;; Return the deep reverse of a-list (reverses all sub-lists).
(define (deep-rev-map a-list)
  (if (atom? a-list)
      a-list
      (map deep-rev-map (reverse a-list))) )

;; Return sum of numbers in number-list.
(define (sum-list-alt number-list)
  (apply + number-list) )

;; Apply 7 to func.
(define (apply-to-7 func)
  (func 7) )

;; Return a list of elt1 and elt2 using cons or list.
(define (listify elt1 elt2)
  ((if (list? elt2) cons list) elt1 elt2) )

;; Return the largest value of the mapping of function onto a-list.
(define (max-of-func function a-list)
  (apply max (map function a-list)) )

;; Return the number of elements in a-list that satisfy func.
(define (count-if func a-list)
  (length (remove #f (map func a-list))) )

;; Return the first element in a-list that satisfies func, else #f.
(define (find-if func a-list)
  (list-ref a-list
            (position #t (map func a-list))) )

;; Return a-list without the elements that satisfy func.
(define (remove-if func a-list)
  (cond ((null? a-list)
         '())
        ((func (first a-list))
         (cons (first a-list)
               (remove-if func (rest a-list))))
        (else
         (remove-if func (rest a-list)))) )

;; Return true if a-list contain a 3 on the top level.
(define (has-3 a-list)
  (member 3 a-list))

;; Return the first element in a-list that satisfies func, else #f.
(define (find-if func a-list)
  (cond ((null? a-list) #f)
        ((func (first a-list)) (first a-list))
        (else (find-if func (rest a-list)))) )

;; Return a-list without the elements that satisfy func.
(define (remove-if func list)
  (cond ((null? list)
         '())
        ((func (first list))
         (remove-if func (rest list)))
        (else
         (cons (first list)
               (remove-if func (rest list))))) )

(define (count-if func a-list)
  (count #t (map func a-list)))

;; Return a list of the positive numbers in a-list.
(define (positive-filter-alt a-list)
  (keep-if positive? a-list) )

(define (remove-duplicates a-list)
  (union
   (remove-if (lambda (elt) (> (count elt a-list) 1) ) a-list)
   (keep-if (lambda (elt) (> (count elt a-list) 1) ) a-list)) )

;; Return true if item is a short list (one or two elements).
(define (short-list? item)
  (and
   (list? item)
   (member (length item) '(1 2))) )

;; Return final true return value if all applications of func
;; to the elements of a-list are true, otherwise return #f.
(define (every func a-list)
  (cond ((null? a-list) #t)
        ((null? (rest a-list)) (func (first a-list)))
        (else (and (func (first a-list))
                   (every func (rest a-list))))) )

;; Return first true application of func to the elements of
;; a-list, otherwise return #f if no applications are true.
(define (any func a-list)
  (if (null? a-list)
      #f
      (or (func (first a-list))
          (any func (rest a-list)))) )

;; Return true if all elements of a-list are numbers.
(define (all-numbers-alt? a-list)
  (every number? a-list) )

;; every and any each take a variable number of lists as arguments
;; and apply func to those lists. apply is used to convert a list of
;; arguments into separate arguments.

;; Return final true return value if all applications of func to
;; successive elements in lists are true, otherwise return #f.
(define (every func . lists)
  (cond ((member #t (map null? lists)) #t)
        ((member #t (map (lambda (lst) (null? (cdr lst))) lists))
         (apply func (map car lists)))
        (else
         (and (apply func (map car lists))
              (apply every (cons func (map cdr lists)))))) )

;; Return first true value from applying func to successive
;; elements in lists, or #f if no elements satisfy func.
(define (any func . lists)
  (if (member #t (map null? lists))
      #f
      (or (apply func (map first lists))
          (apply any (cons func (map rest lists))))) )

;; Return true if elt1 and elt2 (lists or atoms) are equal.
(define (new-equal? elt1 elt2)
  (cond ((and (atom? elt1) (atom? elt2)) (eqv? elt1 elt2))
        ((or (atom? elt1) (atom? elt2)) #f)
        (else (same elt1 elt2))) )

;; Return true if list1 and list2 are equal.
(define (same list1 list2)
  (if (= (length list1) (length list2))
      (every new-equal? list1 list2)
      #f) )

(define (new-equal? elt1 elt2)
  (if (atom? elt1)
      (eqv? elt1 elt2)
      (same elt1 elt2)) )

(define (same list1 list2)
  (if (= (length list1) (length list2))
      (every new-equal? list1 list2)
      #f) )

(define (same elt1 elt2)
  (cond ((and (atom? elt1) (atom? elt2)) (eqv? elt1 elt2))
        ((or (atom? elt1) (atom? elt2)) #f)
        (else (new-equal? elt1 elt2))) )

(define (new-equal? list1 list2)
  (if (= (length list1) (length list2))
      (every same list1 list2)
      #f) )

(define (new-equal? elt1 elt2)
  (cond ((and (atom? elt1) (atom? elt2)) (eqv? elt1 elt2))
        ((or (atom? elt1) (atom? elt2)) #f)
        (else
         (and (= (length elt1) (length elt2))
              (every new-equal? elt1 elt2)))) )

(define (every func . lists)
  (if (any null? lists)
      #t
      (and (apply func (map first lists))
           (apply every (cons func (map rest lists))))) )

(define small-num (lambda (num) (< num 3)))

;; Return the set of items in either set1 or set2.
(define (union-alt set1 set2)
  (append
   set2
   (remove-if
    (lambda (element)
      (member element set2) )
    set1)) )

;; Return the set of items in both set1 and set2.
(define (intersection-alt set1 set2)
  (keep-if
   (lambda (element)
     (member element set2) )
   set1) )

;; Return the first sublist in assoc-list whose car matches element.
(define (alt-assoc element assoc-list)
  (find-if
   (lambda (pair)
     (equal? element (car pair)) )
   assoc-list) )

;; Return sum of numbers in number-list.
(define (add-list number-list)
  (accumulate + number-list) )

;; Return the sum of the absolute values of num1 and num2.
(define (sum-abs num1 num2)
  (+ (abs num1) (abs num2)) )

(define many-lists '((1 2 3) (1 2 3 4 5) (1 2 3 4) (1 2)))

;; Return the longest of list1 and list2.
(define (biggest list1 list2)
  (if (> (length list1) (length list2))
      list1
      list2) )

;; Applies func to answer and first element of a-list, then to that
;; result and next element of a-list and so on until a-list is empty.
;; Returns final answer.
(define (accum-tail func a-list answer)
  (if (null? a-list)
      answer
      (accum-tail func (rest a-list)
                  (func answer (first a-list)))) )

;; Applies func to first two elements of a-list, then to that result
;; and next element of a-list and so on until a-list is empty.
;; Returns final answer.
(define (accumulate func a-list)
  (if (null? a-list)
      (func)
      (accum-tail func (rest a-list) (first a-list))) )

;; Returns the sum of the squares of num1 and num2.
(define (sum-squares num1 num2)
  (+ (* num1 num1) (* num2 num2)) )

;; Returns the sum of total and num squared.
(define (add-num-squared total num)
  (+ total (* num num)) )

;; MIT Scheme: Syntactic keyword may not be used as an expression
;; "and" and "or" are syntax, not procedures
;; (define (find-if func a-list)
;;   (list-ref a-list
;;             (position (accumulate or (map func a-list))
;;                       (map func a-list))) )

(define (flat a-list)
  (if (atom? a-list)
      a-list
      (map flat (accumulate append a-list))) )

(define (flatten a-list)
  (if (atom? a-list)
      (list a-list)
      (accumulate append (map flatten a-list))) )

;; Perform insertion sort on a-list based on compare-func.
(define (sort compare-func a-list)
  (if (null? a-list)
      '()
      (insert
       (first a-list)
       (sort compare-func (rest a-list))
       compare-func)) )

;; Insert element in sorted order into sorted-list based on compare-func.
(define (insert element sorted-list compare-func)
  (cond ((null? sorted-list)
         (list element))
        ((compare-func element (first sorted-list))
         (cons element sorted-list))
        (else
         (cons (first sorted-list)
               (insert element (rest sorted-list) compare-func)))) )

;; Return the value of card (e.g., ten or queen).
(define (card-value card)
  (car card) )

;; Return the suit of card (e.g., diamonds or hearts).
(define (card-suit card)
  (second card) )

;; Construct a card from its value and suit (e.g., (ten hearts)).
(define (create-card value suit)
  (list value suit) )

(define card-ordering
  '(two three four five six seven eight nine ten
        jack queen king ace))

;; Return true if card1 is lower in value than card2.
(define (lower-value? card1 card2)
  (< (position (card-value card1) card-ordering)
     (position (card-value card2) card-ordering)) )

;; Return true if card1 is lower in value than card2.
(define (lower-value? card1 card2)
  (member (card-value card2)
          (rest (member (card-value card1) card-ordering))) )

;; Return true if hand represents a straight.
(define (is-straight-new? hand card-ordering)
  (let* ( (sorted-hand (sort lower-value? hand))
          (low-card (first sorted-hand)) )
    (and (lower-value? low-card (create-card 'jack 'any-suit))
         (equal?
          (map card-value sorted-hand)
          (subseq
           (member (card-value low-card) card-ordering)
           0 5)))) )

;; Return true if hand represents a flush.
(define (is-flush? hand)
  (= 4
     (count
      (card-suit (first hand))
      (map card-suit (rest hand)))) )

;; Return true if hand represents a royal straight.
(define (is-royal-straight? hand)
  (member 'ace (map card-value hand)) )

;; Return the value of a poker hand.
(define (poker-value-new hand)
  (let* ( (hand-values (map card-value hand))
          (count-list
           (map
            (lambda (card)
              (count card hand-values) )
            hand-values)) )
    (cond ((is-straight-new? hand card-ordering)
           (if (is-flush? hand)
               (if (is-royal-straight? hand)
                   'royal-flush
                   'straight-flush)
               'straight))
          ((is-flush? hand)
           'flush)
          ((member 4 count-list)
           'four-of-a-kind)
          ((and (member 3 count-list) (member 2 count-list))
           'full-house)
          ((member 3 count-list)
           'three-of-a-kind)
          ((= 4 (count 2 count-list))
           'two-pair)
          ((member 2 count-list)
           'one-pair)
          (else
           'nothing))) )

(define (is-flush? hand)
  (let ((first-suit (card-suit (first-hand))))
    (every (lambda (card)
             (eqv? first-suit (card-suit card)))
           (rest hand))) )

(define (is-flush? hand)
  (let ((first-suit (card-suit (first-hand))))
    (= 4
       (count-if (lambda (card)
                   (eqv? first-suit (card-suit card)))
                 (rest hand)))) )

(define (sel-sort a-list compare)
  (if (null? a-list)
      '()
      (let ((next-value (accumulate compare a-list)))
        (cons next-value
              (sel-sort (remove next-value a-list) compare)))) )
