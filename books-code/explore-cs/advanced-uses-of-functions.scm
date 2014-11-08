;; Code from Chapter 11 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

;; Return average of a variable amount of numbers.
(define (avg . nums)
  (if (null? nums)
      'no-average
      (/ (apply + nums) (length nums))) )

;; Return list of elements specified by their positions.
(define (elts a-list . positions)
  (map (lambda (pos) (list-ref a-list pos)) positions) )

(define avg
  (lambda nums
    (if (null? nums)
        'no-average
        (/ (apply + nums) (length nums))) ) )

(define elts
  (lambda (a-list . positions)
    (map (lambda (pos) (list-ref a-list pos) ) positions) ) )

;; Return function encapsulating info about a person.
(define (person-info name birth-date income job)
  (lambda (request)
    (cond ((eq? request 'name) name)
          ((eq? request 'age) (- current-year (third birth-date)))
          ((eq? request 'year) (third birth-date))
          ((eq? request 'month) (first birth-date))
          ((eq? request 'day) (second birth-date))
          ((eq? request 'income) income)
          ((eq? request 'broke) (< income 1000))
          ((eq? request 'rich) (> income 100000))
          ((eq? request 'occupation) job)) ) )

(define dilbert
  (person-info 'dilbert '(5 12 57) 45000 'programmer))

(define dogbert (person-info 'dogbert '(7 9 90) 0 'philosopher))

(define studbert dilbert)

;; Return function encapsulating job information.
(define (job-info name income job)
  (lambda (request . value)
    (cond ((eq? request 'name) name)
          ((eq? request 'income) income)
          ((eq? request 'broke) (< income 1000))
          ((eq? request 'rich) (> income 100000))
          ((eq? request 'occupation) job)
          ((eq? request 'raise)
           (set! income (+ income (first value))))
          ((eq? request 'new-occupation)
           (set! job (first value)))) ) )

(define ratbert (job-info 'ratbert 1000 'pest))

(define larry (job-info 'larry 10000 'stooge))
(define moe (job-info 'moe 11000 'stooge))
(define curly (job-info 'curly 12000 'stooge))
(define emps (list larry moe curly))

(define (weird func)
  (lambda args (list (apply func args)) ) )

;; Create simple car class.
(define (auto make model)
  (lambda (req)
    (cond ((eq? req 'make) make)
          ((eq? req 'model) model)
          (else 'bad-request)) ) )

(define fast (auto 'porsche 928))
(define small (auto 'plymouth 'arrow))

;; Create car class with modifiable instance variables.
(define (auto make model color accessories)
  (lambda (req . args)
    (cond ((eq? req 'make) make)
          ((eq? req 'model) model)
          ((eq? req 'color) color)
          ((eq? req 'paint) (set! color (first args)) color)
          ((eq? req 'accessories) accessories)
          ((eq? req 'add)
           (set! accessories (append args accessories))
           accessories)
          (else 'bad-request)) ) )

(define fast (auto 'porsche 928 'red '(stereo fat-tires)))
(define small (auto 'plymouth 'arrow 'white '()))

;; Create car class with class and instance variables.
(define (auto make model)
  (let ((all-repair-cost 0))
    (lambda (req . args)
      (let ((repaired '()))
        (cond ((eq? req 'make) make)
              ((eq? req 'model) model)
              ((eq? req 'cost) all-repair-cost)
              ((eq? req 'repair)
               (set! all-repair-cost
                     (+ (second args) all-repair-cost))
               (set! repaired (cons (first args) repaired))
               repaired)
              (else 'bad-request))) )) )

(define fast (auto 'porsche 928))
(define small (auto 'plymouth 'arrow))

;; Create car class with class and instance variables.
(define auto
  (let ((all-repair-cost 0))   ; class variable
    (lambda (make model)       ; auto function header
      (let ((repaired '()))    ; instance variable
        (lambda (req . args)   ; parameters for methods
          (cond ((eq? req 'make) make)
                ((eq? req 'model) model)
                ((eq? req 'cost) all-repair-cost)
                ((eq? req 'repair)
                 (set! all-repair-cost
                       (+ (second args) all-repair-cost))
                 (set! repaired (cons (first args) repaired))
                 repaired)
                (else 'bad-request)) )) )) )

(define fast (auto 'porsche 928))
(define small (auto 'plymouth 'arrow))

;; Create vehicle superclass.
(define (vehicle make model)
  (lambda (req)
    (cond ((eq? req 'make) make)
          ((eq? req 'model) model)
          (else 'bad-request)) ) )

;; Create car subclass.
(define (auto num-doors . args)
  (let ((parent (apply vehicle args)))   ; create vehicle instance
    (lambda (req . args)
      (cond ((eq? req 'num-doors) num-doors)
            (else   ; send message to parent
             (apply parent (cons req args)))) )) )

(define fast (auto 3 'porsche 928))
(define small (auto 2 'plymouth 'arrow))

;; Create vehicle superclass.
(define vehicle
  (let ((num 0))
    (lambda (type make model year color owner)
      (set! num (+ num 1))
      (lambda (req . args)
        (cond ((eq? req 'type) type)
              ((eq? req 'make) make)
              ((eq? req 'model) model)
              ((eq? req 'year) year)
              ((eq? req 'color) color)
              ((eq? req 'owner) owner)
              ((eq? req 'buy) (set! owner (car args)) owner)
              ((eq? req 'count) num)
              (else 'bad-request)) ) )) )

;; Create car subclass.
(define (auto num-doors . args)
  (let ((parent (apply vehicle (cons 'car args))))
    (lambda (req . args)
      (cond ((eq? req 'num-doors) num-doors)
            (else (apply parent (cons req args)))) )) )

;; Create motorcycle subclass.
(define motorcycle
  (let ((num-bikes 0))
    (lambda args
      (set! num-bikes (+ num-bikes 1))
      (let ((parent (apply vehicle (cons 'motorcycle args))))
        (lambda (req . args)
          (cond ((eq? req 'num-bikes) num-bikes)
                (else (apply parent (cons req args)))) )) )) )

(define my-car (auto 4 'vw 'jetta 1984 'blue 'oliver))
(define her-car (auto 3 'mazda 323 1990 'blue 'myriam))
(define old-bike (motorcycle 'yamaha 'XS400 1988 'white 'oliver))
(define new-bike (motorcycle 'kawasaki 'KZ650 1996 'red 'gino))
(define van (vehicle 'utility 'nissan 'quest 1996 'silver 'hans))
(define all (list my-car her-car old-bike new-bike van))

;; Print messages during recursive descent and unwind.
(define (vanilla arg)
  (cond ((> arg 3) 'done)
        (else
         (display "before recursion")
         (newline)
         (vanilla (+ arg 1))
         (display "after recursion")
         (newline))) )

;; Print messages during recursive descent only.
(define (strawberry arg)
  (call-with-current-continuation
   (lambda (stop)
     (define (inner-berry arg)
       (cond ((> arg 3) (stop 'done))
             (else
              (display "before recursion")
              (newline)
              (inner-berry (+ arg 1))
              (display "after recursion")
              (newline))) )
     (inner-berry arg))) )

;; Set up exit function and pass to nonberry.
(define (chocolate arg)
  (call-with-current-continuation
   (lambda (stop)
     (nonberry arg stop))) )

;; Print messages during recursive descent only.
(define (nonberry arg exit-func)
  (cond ((> arg 3) (exit-func 'done))
        (else
         (display "before recursion")
         (newline)
         (nonberry (+ arg 1) exit-func)
         (display "after recursion")
         (newline))) )

(define bail-out 'nothing-yet)

;; Return #t if all elements in set1 are also in set2, #f otherwise.
(define (subset?-iter set1 set2)
  (call-with-current-continuation
   (lambda (exit)
     (do ( (test-set set1 (rest test-set)) )
         ((null? test-set) #t)
       (if (not (member (first test-set) set2))
           (exit #f))))) )

;; Return the division and department of person in company-list,
;; #f if person is not in company-list.
(define (find-employee company-list person)
  (call-with-current-continuation
   (lambda (return)
     (do ( (company company-list (rest company)) )
         ((null? company) #f)
       (do ( (dept (department-list (first company)) (rest dept)) )
           ((null? dept) 'no-match)
         (if (member person (employees (first dept)))
             (return (list (division-name (first company))
                           (department-name (first dept))))))) )) )

(define (strawberry arg)
  (call-with-current-continuation
   (lambda (stop)
     (cond ((> arg 3) (stop 'done))
           (else
            (display "before recursion")
            (newline)
            (strawberry (+ arg 1))
            (display "after recursion")
            (newline))))) )
