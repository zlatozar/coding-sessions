;; Code from Chapter 3 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

;; Return the square of a number.
(define (square number)
  (* number number) )

(define (sum-abs num1 num2)     ; proper heading
  (+ (abs num1) (abs num2)) )

;; Return amount of tax given income, deductions, and tax credits.
(define (tax-amount work-income interest-income standard-deduction
                    withholding tax-credits)
  (- (* (+ work-income interest-income (- standard-deduction))
        0.15)
     (+ withholding tax-credits)) )

;; Compute tax based on three income brackets.
(define (tax-rate income)
  (+ (* (min income 20000) 0.15)
     (* (max (- (min income 50000) 20000) 0) 0.25)
     (* (max (- income 50000) 0) 0.5)) )

;; Return amount of tax given income, deductions, and tax credits.
(define (tax-amount work-income interest-income standard-deduction
                    withholding tax-credits)
  (- (tax-rate
      (+ work-income interest-income (- standard-deduction)))
     (+ withholding tax-credits)) )

;; Return difference between the largest and smallest of three
;; numbers within the range min to max.
(define (difference num1 num2 num3 min max)
  (- (max num1 num2 num3 min) (min num1 num2 num3 max)))

(define number 4)

(define (double number)
  (* 2 number) )

(define (new-double)
  (* 2 number) )

;; In MIT Scheme it is an error:
;;   "Premature reference to reserved name: number"

;; Redefine number to be twice as large.
;; (define (bad-double)
;;   (define number (* 2 number)) )

;; Return amount of tax given income, deductions, and tax credits.
(define (tax-amount work-income interest-income standard-deduction
                    withholding tax-credits)
  (let ( (total-debits (* (+ work-income interest-income
                             (- standard-deduction))
                          0.15))
         (total-credits (+ withholding tax-credits)) )
    (- total-debits total-credits)) )

;; Return amount of tax given income, deductions, and tax credits.
(define (tax-amount work-income interest-income standard-deduction
                    withholding tax-credits)
  (let* ( (taxable-income (+ work-income interest-income
                             (- standard-deduction)))
          (total-debits (* taxable-income 0.15))
          (total-credits (+ withholding tax-credits)) )
    (- total-debits total-credits)) )

(define (piggy-bank pennies)
  (let ((quarters (truncate (/ pennies 25)))
        (nickels (truncate (/ pennies 5)))
        (left-over-pennies (remainder pennies 1)))
    (list quarters nickels left-over-pennies)))

(define (this-books-style arg1 arg2)
  (let ( (var1 value1)
         (var2 value2) )
    (+ (some-very-long-function with lots of arguments)
       3)) )

;; style example 1
(define (line-up-parens-style2 arg1 arg2)
  (let ( (var1 value1)
         (var2 value2)
         )
    (+ (some-very-long-function with lots of arguments)
       3
       )
    )
  )

;; style example 2
(define
  (arguments-on-lines-below-style arg1 arg2)
  (let
      ( (var1 value1)
        (var2 value2) )
    (+
     (some-very-long-function
      with
      lots
      of
      arguments)
     3)) )
