;; Code from Chapter 6 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

(load "extensions.scm")

;; Return the first number greater than threshold in a-list.
(define (first-greater threshold a-list)
  (if (> (first a-list) threshold)
      (first a-list)
      (first-greater threshold (rest a-list))) )

;; Return the first number greater than threshold in a-list.
(define (first-greater threshold a-list)
  (cond ((> (first a-list) threshold)
         (first a-list))
        ((null? a-list)
         #f)
        (else
         (first-greater threshold (rest a-list)))) )

;; Return the first number greater than threshold in a-list.
(define (first-greater threshold a-list)
  (cond ((null? a-list)
         #f)
        ((> (first a-list) threshold)
         (first a-list))
        (else
         (first-greater threshold (rest a-list)))) )

;; Compute growth of investment given start balance, time period,
;; and daily interest rate with increment added every 30 days.
(define (new-balance balance counter days rate increment)
  (cond ((> counter days)
         balance)
        ((zero? (remainder counter 30))
         (new-balance
          (+ (* balance rate) balance increment)
          (+ counter 1) days rate increment))
        (else
         (new-balance
          (+ (* balance rate) balance)
          (+ counter 1) days rate increment))) )

;; Compute annual investment given annual interest rate, years,
;; and monthly investment amount.
(define (investment years rate increment)
  (new-balance 0 1 (* 365 years) (/ rate 365) increment) )

;; Return sum of the digits in number.
(define (sum-digits number)
  (if (zero? (truncate (/ number 10)))
      number
      (+ (remainder number 10)
         (sum-digits (truncate (/ number 10))))) )

;; Return sum of the digits in number.
(define (sum-digits number)
  (let ( (last-digit (remainder number 10))
         (rest-of-number (truncate (/ number 10))) )
    (if (zero? rest-of-number)
        number
        (+ last-digit
           (sum-digits rest-of-number)))) )

;; Return true if the digits in number are increasing
;; from left to right.
(define (increasing-digits number)
  (let* ( (last-digit (remainder number 10))
          (rest-of-number (truncate (/ number 10)))
          (next-to-last-digit (remainder rest-of-number 10)) )
    (cond ((zero? rest-of-number) #t)
          ((> next-to-last-digit last-digit) #f)
          (else (increasing-digits rest-of-number)))) )

(define (new-balance balance counter days rate period increment)
  (cond ((> counter days)
         balance)
        ((zero? (remainder counter period))
         (new-balance
          (+ (* balance rate) balance increment)
          (+ counter 1)
          days rate period increment))
        (else
         (new-balance
          (+ (* balance rate) balance)
          (+ counter 1)
          days rate period increment))) )

(define (newer-balance balance counter days rate day-list
                       increment)
  (cond ((> counter days)
         balance)
        ((member (remainder counter 7) day-list)
         (newer-balance
          (+ (* balance rate) balance increment)
          (+ counter 1)
          days rate day-list increment))
        (else (newer-balance
               (+ (* balance rate) balance)
               (+ counter 1)
               days rate day-list increment))) )

;; Compute growth of investment given start balance, time period,
;; and daily interest rate with increment added every 30 days.
(define (alt-new-balance balance counter)
  (cond ((> counter days)
         balance)
        ((zero? (remainder counter 30))
         (new-balance
          (+ (* balance rate) balance increment)
          (+ counter 1)))
        (else
         (new-balance
          (+ (* balance rate) balance)
          (+ counter 1)))) )

;; Return max factorial.
(define (factorial max)
  (if (zero? max)
      1
      (* max
         (factorial (- max 1)))) )

;; Return max factorial (tail recursive).
(define (tail-factorial max total)
  (if (zero? max)
      total
      (tail-factorial (- max 1) (* max total))) )

;; Return max factorial (helper function).
(define (fact max)
  (tail-factorial max 1))

;; Return sum of numbers in number-list.
(define (sum-list number-list)
  (if (null? number-list)
      0
      (+ (first number-list)
         (sum-list (rest number-list)))) )

;; Return true if all elements of a-list are numbers.
(define (all-numbers? a-list)
  (cond ((null? a-list)
         #t)
        ((not (number? (first a-list)))
         #f)
        (else
         (all-numbers? (rest a-list)))) )

;; Add all numbers in a-list unless some are not numbers.
(define (safe-sum a-list)
  (if (all-numbers? a-list)
      (sum-list a-list)
      'bad-list) )

(define (abc xyz)
  (cond ((first xyz) (rest xyz))
        (else (abc xyz))) )

(define (def uvw)
  (or (zero? (first uvw)) (def (rest uvw))) )

(define (all-numbers-alt? a-list)
  (if (null? a-list)
      #t
      (and (number? (first a-list))
           (all-numbers-alt? (rest a-list)))) )

(define (all-numbers-alt? a-list)
  (if (null? a-list)
      #t
      (and (all-numbers-alt? (rest a-list))
           (number? (first a-list)))) )

(define (unknown a-list c1 c2 c3)
  (cond ((null? a-list)
         (list c1 c2 c3))
        ((number? (first a-list))
         (unknown (rest a-list) (+ c1 1) c2 c3))
        ((symbol? (first a-list))
         (unknown (rest a-list) c1 (+ c2 1) c3))
        ((list? (first a-list))
         (unknown (rest a-list) c1 c2 (+ c3 1)))
        (else
         (unknown (rest a-list) c1 c2 c3))) )

(define (mystery a-list)
  (cond ((null? a-list) #f)
        ((atom? a-list) a-list)
        ((symbol? (first a-list)) (first a-list))
        (else
         (mystery (list-ref a-list (first a-list))))) )

(define (abc lst)
  (cond ((null? lst) lst)
        ((>= (def (car lst)) (def (abc (cdr lst))))
         (car lst))
        (else (abc (cdr lst)))) )

(define (def lst)
  (if (null? lst)
      0
      (+ 1 (def (cdr lst)))) )

(define (abc lst)
  (cond ((null? lst) 0)
        ((>= (def (car lst)) (abc (cdr lst)))   (def (car lst)))
        (else (abc (cdr lst)))))

;; Return a list of the square roots of the numbers in a-list.
(define (square-roots a-list)
  (if (null? a-list)
      '()
      (cons (sqrt (first a-list))
            (square-roots (rest a-list)))) )

;; Return a list of the positive numbers in a-list.
(define (positive-filter a-list)
  (cond ((null? a-list)
         '())
        ((positive? (first a-list))
         (cons (first a-list)
               (positive-filter (rest a-list))))
        (else
         (positive-filter (rest a-list)))) )

(define (switch-em a-list)
  (if (null? a-list)
      '()
      (append
       (second a-list)
       (first a-list)
       (switch-em (cdr a-list)))) )

;; Return the number of atoms that occur anywhere in a-list.
(define (count-atoms a-list)
  (cond ((null? a-list)
         0)
        ((atom? (car a-list))
         (+ 1 (count-atoms (cdr a-list))))
        (else
         (+ (count-atoms (car a-list))
            (count-atoms (cdr a-list))))) )

;; Return the deep reverse of a-list (reverses all sub-lists).
(define (deep-reverse a-list)
  (cond ((null? a-list)
         '())
        ((atom? (car a-list))
         (append (deep-reverse (cdr a-list))
                 (list (car a-list))))
        (else
         (append (deep-reverse (cdr a-list))
                 (deep-reverse (car a-list))))) )

;; Return the deep reverse of a-list (reverses all sub-lists).
(define (deep-reverse a-list)
  (cond ((null? a-list)
         '())
        ((atom? (car a-list))
         (append (deep-reverse (cdr a-list))
                 (list (car a-list))))
        (else
         (append (deep-reverse (cdr a-list))
                 (list (deep-reverse (car a-list)))))) )

(define (count-atoms a-list)
  (cond ((null? a-list)
         0)
        ((atom? a-list)
         1)
        (else
         (+ (count-atoms (car a-list))
            (count-atoms (cdr a-list))))) )

(define (deep-reverse a-list)
  (cond ((null? a-list)
         '())
        ((atom? a-list)
         a-list)
        (else
         (append (deep-reverse (cdr a-list))
                 (list (deep-reverse (car a-list)))))) )

(define (unknown a-list)
  (cond ((null? a-list) '())
        ((number? a-list) (list a-list))
        ((symbol? a-list) '())
        (else
         (append
          (unknown (car a-list))
          (unknown (cdr a-list))))) )

(define (mystery unknown)
  (if (or (null? unknown) (atom? unknown))
      unknown
      (cons (mystery (car unknown)) (mystery (cdr unknown)))) )

;; Return max factorial.
(define (factorial max)
  (if (zero? max)
      1
      (* max
         (factorial (- max 1)))) )

;; Return sum of 0 through number factorial.
(define (sum-facts number)
  (if (zero? number)
      0
      (+ (factorial number)
         (sum-facts (- number 1)))) )

(define CD-list        ; (not written as a define in the text)
  '( ( rock
       ( (Rolling Stones)
         (Black and Blue)
         (Its Only Rock and Roll) ) )
     ( jazz
       ( (Pat Metheny)
         (First Circle) )
       ( (Andy Narell)
         (The Hammer) ) ) ) )

;; Find the CDs of artist-name within a music category list.
(define (CDs-within-category-inner artist-list artist-name)
  (cond ((null? artist-list)
         #f)
        ((equal? (first (first artist-list)) artist-name)
         (rest (first artist-list)))
        (else
         (CDs-within-category (rest artist-list) artist-name))) )

;; Find the CDs of artist-name within the entire CD-collection.
(define (CDs CD-collection artist-name)
  (cond ((null? CD-collection)
         #f)
        ((CDs-within-category-inner (rest (first CD-collection))
                              artist-name)
         (CDs-within-category-inner (rest (first CD-collection))
                              artist-name))
        (else
         (CDs (rest CD-collection) artist-name))) )

;; Find the CDs of artist-name within the entire CD-collection using 'or'
(define (CDs CD-collection artist-name)
  (cond ((null? CD-collection)
         #f)
        (else
         (or (CDs-within-category-inner
              (rest (first CD-collection))
              artist-name)
             (CDs (rest CD-collection) artist-name)))) )

;; Find CD-name by an artist within a music category list.
(define (CDs-within-category artist-list CD-name)
  (cond ((null? artist-list)
         #f)
        (else
         (or (CD-within-CD-list
              (rest (first artist-list))
              CD-name)
             (CDs-within-category
              (rest artist-list) CD-name)))) )

;; Find CD-name within a list of CDs.
(define (CD-within-CD-list CD-list CD-name)
  (cond ((null? CD-list)
         #f)
        ((equal? (first CD-list) CD-name)
         CD-name)
        (else
         (CD-within-CD-list (rest CD-list) CD-name))) )

;; Find CD-name within a list of CDs.
(define (CD-within-CD-list CD-list CD-name)
  (let ( (found-name (member CD-name CD-list)) )
    (if found-name
        (first found-name)
        #f)) )

(define card-ordering
  '(two three four five six seven eight nine ten
        jack queen king ace))

;; Return true if card1 is lower in value than card2.
(define (lower-card? card1 card2)
  (< (position card1 card-ordering)
     (position card2 card-ordering)) )

;; Insert card in sorted order into sorted-list.
(define (insert-card element sorted-list)
  (cond ((null? sorted-list)
         (list element))
        ((lower-card? element (first sorted-list))
         (cons element sorted-list))
        (else
         (cons (first sorted-list)
               (insert-card element (rest sorted-list))))) )

;; Perform insertion sort on a-list.
(define (sort-hand a-list)
  (if (null? a-list)
      '()
      (insert-card
       (first a-list)
       (sort-hand (rest a-list)))) )

;; Return true if hand is a straight.
(define (is-straight? hand card-ordering)
  (let* ( (sorted-hand (sort-hand hand))
          (low-card (first sorted-hand)) )
    (and (lower-card? low-card 'jack)
         (equal?
          sorted-hand
          (subseq
           (member low-card card-ordering)
           0 5)))) )

(define (CDs CD-collection artist-name)
  (or
   (null? CD-collection)
   (CDs-within-category (rest (first CD-collection))
                        artist-name)
   (CDs (rest CD-collection) artist-name)) )
