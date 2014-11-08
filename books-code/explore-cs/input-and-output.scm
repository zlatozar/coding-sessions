;; Code from Chapter 9 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

;; Print out num and return num squared.
(define (print-and-square num)
  (display num)
  (* num num) )

;; Print out num, move to the next line, and return num squared.
(define (print-and-square num)
  (display num)
  (newline)
  (* num num) )

;; Print elements of a-list one per line.
(define (one-per-line a-list)
  (for-each
   (lambda (item)
     (display item)
     (newline))
   a-list) )

;; Read in a value and return its square root.
(define (read-and-apply)
  (sqrt (read)) )

;; Read in values until a number is entered; return its square root.
(define (get-number)
  (display "Enter a number: ")
  (if (number? (read))
      (sqrt (read))
      (get-number)) )

;; Read in values until a number is entered; return its square root.
(define (get-number)
  (display "Enter a number: ")
  (let ( (number (read)) )
    (if (number? number)
        (sqrt number)
        (get-number))) )

;; Read in values until a yes or no is entered; return #t if
;; yes is entered and #f if no is entered.
(define (yes-no query)
  (display query)
  (display " (yes or no) ")
  (let ( (answer (read)) )
    (cond ((eqv? answer 'yes) #t)
          ((eqv? answer 'no) #f)
          (else (yes-no query)))) )

;; Count the number of times element occurs in a-list; print an
;; informative message about the count and return the count.
(define (number-of-times element a-list)
  (cond ((null? a-list)
         (display "The list is empty ")
         0)
        ((not (member element a-list))
         (display "The item did not occur in the list ")
         0)
        (else
         (display "The number of times item occurs in the list is ")
         (count element a-list))) )

;; Model the growth of population organisms for times generations.
;; growth is the growth rate.
(define (population-growth times growth population)
  (cond ((= 0 times)
         population)
        (else
         (display population)
         (newline)
         (population-growth (- times 1) growth
                            (* growth population (- 1 population))))) )

(define (population-growth times growth population)
  (cond ((= 0 times)
         population)
        (else
         (population-growth (- times 1) growth
                            (* growth population (- 1 population)))
         (display population)
         (newline))) )

;; Print prompt, read input, print out evaluation, repeat.
(define (read-eval-print)
  (display "-> ")
  (write (eval (read)))
  (newline)
  (read-eval-print) )

;; Print prompt, read input, print out evaluation, repeat until
;; quit is entered.
(define (read-eval-print-with-exit)
  (display "-> ")
  (let ( (command (read)) )
    (cond ((eqv? command 'quit)
           'bye)
          (else
           (write (eval command))
           (newline)
           (read-eval-print-with-exit)))) )

(define (mystery num)
  (cond ((zero? num) 0)
        (else (display num)
              (newline)
              (mystery (- num 1)))) )

(define (unknown num)
  (cond ((zero? num) 0)
        (else (unknown (- num 1))
              (display num)
              (newline))) )

(define (average num-list)
  (/ (display (accumulate + num-list))
     (length num-list)) )
