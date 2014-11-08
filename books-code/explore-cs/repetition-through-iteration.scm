;; Code from Chapter 10 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

;; Infinite loop with embedded recursion.
(define (infinite)
  (infinite)
  0 )

;; Infinite loop with tail recursion.
(define (infinite-iter)
  (infinite-iter) )

;; Model the growth of population organisms for times generations.
;; growth is the growth rate
(define (population-growth-iter times growth population)
  (do ( (counter times (- counter 1))
        (pop population (* growth pop (- 1 pop))) )
      ((= counter 0) pop)
    (display pop)
    (newline)) )

;; Return true if all elements of a-list are numbers.
(define (all-numbers?-iter a-list)
  (do ( (current-list a-list (rest current-list)) )
      ((or (null? current-list)
           (not (number? (first current-list))))
       (if (null? current-list)
           #t
           #f))) )

;; Return true if all elements of a-list are numbers - buggy.
(define (all-numbers?-iter-bad a-list)
  (do ( (current-list a-list (rest current-list)) )
      ((null? current-list) #t)
    (if (not (number? (first current-list)))
        #f)) )

;; Return max factorial (with iteration).
(define (fact-iter max)
  (do ( (number max (- number 1))
        (prod 1 (* number prod)) )
      ((zero? number) prod)) )

;; Return sum of the digits in number.
(define (sum-digits-iter number)
  (do ( (last-digit (remainder number 10)
                    (remainder rest-of-number 10))
        (rest-of-number (truncate (/ number 10))
                        (truncate (/ rest-of-number 10)))
        (answer last-digit (+ answer last-digit)) )
      ((zero? rest-of-number) answer)) )

;; Return sum of the digits in number.
(define (sum-digits-iter number)
  (do ( (last-digit (remainder number 10)
                    (remainder rest-of-number 10))
        (rest-of-number (truncate (/ number 10))
                        (truncate (/ rest-of-number 10)))
        (answer 0 (+ answer last-digit)) )
      ((zero? rest-of-number) answer)) )

;; Return sum of the digits in number.
(define (sum-digits-iter number)
  (do ( (rest-of-number (truncate (/ number 10))
                        (truncate (/ rest-of-number 10)))
        (answer (remainder number 10)
                (+ answer (remainder rest-of-number 10))) )
      ((zero? rest-of-number) answer)) )

;; Return a list of the square roots of the numbers in a-list.
(define (square-roots-iter a-list)
  (do ( (current-list a-list (cdr current-list))
        (answer '() (cons (sqrt (car current-list)) answer)) )
      ((null? current-list) answer)) )

;; Return a list of the square roots of the numbers in a-list.
(define (square-roots-iter-correct a-list)
  (do ( (current-list a-list (cdr current-list))
        (answer '() (cons (sqrt (car current-list)) answer)) )
      ((null? current-list) (reverse answer))) )

;; Return a list of the positive numbers in a-list.
(define (positive-filter-iter a-list)
  (do ( (current-list a-list (cdr current-list))
        (answer '() (if (positive? (car current-list))
                        (cons (car current-list) answer)
                        answer)) )
      ((null? current-list) (reverse answer))) )

;; Perform insertion sort on a-list based on compare-func.
(define (sort-iter compare-func a-list)
  (do ( (current-list a-list (rest current-list))
        (sorted-list '() (insert-iter (first current-list)
                                      sorted-list compare-func)) )
      ((null? current-list) sorted-list)) )

;; Insert element in sorted order into sorted-list based on
;; compare-func.
(define (insert-iter element sorted-list compare-func)
  (do ( (sort-list sorted-list (rest sort-list))
        (new-list '() (if (compare-func element (first sort-list))
                          (cons element new-list)
                          (cons (first sort-list) new-list))) )
      ((or (null? sort-list)
           (compare-func element (first sort-list)))
       (if (null? sort-list)
           (reverse new-list)
           (append (reverse new-list) sort-list)))) )

;; Insert element in sorted order into sorted-list based on
;; compare-func.
(define (insert-iter element sorted-list compare-func)
  (do ( (sort-list sorted-list (rest sort-list))
        (new-list '() (cons (first sort-list) new-list)) )
      ((or (null? sort-list)
           (compare-func element (first sort-list)))
       (if (null? sort-list)
           (reverse new-list)
           (append (reverse new-list) (list element) sort-list)))) )

;; Insert element in sorted order into sorted-list based on
;; compare-func.
(define (insert-iter element sorted-list compare-func)
  (do ( (sort-list sorted-list (rest sort-list))
        (new-list '() (cons (first sort-list) new-list)) )
      ((or (null? sort-list)
           (compare-func element (first sort-list)))
       (if (null? sort-list)
           (reverse (cons element new-list))
           (append (reverse new-list) (list element) sort-list)))) )

(define com                     ; (not written as a define in the text)
  '((far-east (engineering gino bill)
              (advertising bernice yoshiro kumi))
    (eastern  (health ximena)
              (technical eric seth))
    (western  (engineering brian ephram robert)
              (investment stephen))
    (european (management maria)
              (sales hans)
              (advertising jutta jurgen tiziana))))

;; Return the name of division.
(define (division-name division)
  (first division) )

;; Return the list of departments in division.
(define (department-list division)
  (rest division) )

;; Return the name of dept.
(define (department-name dept)
  (first dept) )

;; Return the list of employees in dept.
(define (employees dept)
  (rest dept) )

;; Return the division and department of person in company-list,
;; #f if person is not in company-list.
(define (find-employee company-list person)
  (do ( (company company-list (rest company))
        (dept
         (find-dept (department-list (first company-list)) person)
         (find-dept (department-list (first company)) person)) )
      ((or (null? company) dept)
       (if (null? company)
           #f
           (list (division-name (first company))
                 dept)))) )

;; Return the department of person in dept-list, #f if person is not
;; in dept-list.
(define (find-dept dept-list person)
  (do ( (dept dept-list (rest dept)) )
      ((or (null? dept)
           (member person (employees (first dept))))
       (if (null? dept)
           #f
           (department-name (first dept))))) )

;; Return the division and department of person in company-list,
;; #f if person is not in company-list.
(define (find-employee company-list person)
  (do ( (company company-list (rest company)) )
      ((or (null? company)
           (find-dept (department-list (first company)) person))
       (if (null? company)
           #f
           (list (division-name (first company))
                 (find-dept (department-list (first company))
                            person))))) )

(define (find-employee company-list person)
  (do ( (company company-list (rest company))
        (dept #f
              (find-dept (first company) person)) )
      ((or (null? company) dept)
       (if (null? company)
           #f
           dept))) )

(define (find-dept division person)
  (do ( (dept (department-list division) (rest dept)) )
      ((or (null? dept)
           (member person (employees (first dept))))
       (if (null? dept)
           #f
           (list (division-name division)
                 (department-name (first dept)))))) )
