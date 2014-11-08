;; Code from Chapter 4 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

;; Return the first element of a list. 
(define (first a-list)
  (car a-list) )

;; Return the rest of a list. 
(define (rest a-list)
  (cdr a-list) )

;; Return the second element of a list. 
(define (second a-list)
  (car (cdr a-list)) )

;; Return the third element of a list. 
(define (third a-list)
  (car (cdr (cdr a-list))) )

;; Return the last element in a-list. 
(define (last a-list)
  (list-ref 'a-list (- (length 'a-list) 1)) )

;; Return the last element in a-list. 
(define (last a-list)
  (list-ref (a-list) (- (length (a-list)) 1)) )

;; Return the last element in a-list. 
(define (last a-list)
  (list-ref '(a-list) (- (length '(a-list)) 1)) )

;; Return the last element in a-list. 
(define (last a-list)
  (list-ref a-list (- (length a-list) 1)) )

(define retort
  '((i am sorry but we are closed now)
    (talk to the person at the end of the hall)
    (you need form 1044-tx8 and not 1044-fg4)
    (we cannot take personal checks)
    (i am sorry we need exact change)
    (oh you only had to fill out this one form not those 20 others)))

;; Return a random element from the list a-list. 
(define (get-random-element a-list)
  (list-ref 
   a-list
   (random (length a-list))) )

(define (average1 num1 num2 num3)
  (/ (+ num1 num2 num3) 3) )

(define (average2 num-list)
  (/ (+ (first num-list) (second num-list)
        (third num-list)) 3) )

(define (month month-num)
  (list-ref '(January February March April May June July
                      August September October November December)
            month-num) )

(define (replace-element a-list position element)
  (append 
   (subseq a-list 0 position)
   element
   (subseq a-list position)) )

;; Return a-list with item added to the end. 
(define (add-to-end item a-list)
  (append a-list (list item)) )

;; Return a sentence with noun-phrase, verb-phrase,
;; and "in the dark."
(define (in-the-dark-sentence noun-phrase verb-phrase)
  (append 
   noun-phrase
   verb-phrase
   '(in the dark)) )

;; Return a sentence with noun-phrase, "dances,"
;; and object-phrase. 
(define (dances-sentence noun-phrase object-phrase)
  (append 
   noun-phrase
   '(dances)
   object-phrase) )

;; Return the last name, fourth element, from a name list. 
(define (last-name name-list)
  (fourth name-list) )

;; Return the prefix (first element) from a name list. 
(define (prefix name-list)
  (first name-list) )

;; Produce a form letter addressed to name-list. 
(define (make-form-letter name-list)
  (list 
   (list 'Dear (prefix name-list) (last-name name-list))
   '()
   '(This is your last chance to receive our mailings at)
   (list 'the (last-name name-list) 'residence. 'By 'ordering
         'your 'personalized)
   (list 'ceramic 'utensil 'set, 'we 'will 'enter 'the
         (last-name name-list) 'family)
   '(in our sweepstakes giveaway. Don't think any more,)
   '(just do it.)) )

;; Produce a form letter addressed to name-list. 
(define (make-form-letter name-list)
  (list 
   (append '(Dear) (prefix name-list) (last-name name-list))
   '()
   '(This is your last chance to receive our mailings at)
   (append '(the) (last-name name-list)
           '(residence. By ordering your personalized))
   (append '(ceramic utensil set we will enter the)
           (last-name name-list) '(family))
   '(in our sweepstakes giveaway. Don't think any more,)
   '(just do it.)) )

;; Return the number of times item occurs in a-list. 
(define (count item a-list)
  (- (length a-list)
     (length (remove item a-list))) )

;; Return the position of item in a-list. 
(define (position item a-list)
  (- (length a-list)
     (length (member item a-list))) )

(define months
  '(jan feb mar apr may jun jul aug sep oct nov dec))

(define days
  '( 31  28  31  30  31  30  31  31  30  31  30  31))

(define CD-list            ; (not written as a define in the text)
  '(rock
    (Rolling_Stones
     (Black_and_Blue
      Its_Only_Rock_and_Roll))
    jazz
    (Pat_Metheny
     (First_Circle)
     Andy_Narell
     (The_Hammer))))

;; Return the element that follows selector in a-list. 
(define (element-after selector a-list)
  (second (member selector a-list)) )

;; Return the CDs by artist and type in CD-list. 
(define (artist-CD-list type artist CD-list)
  (element-after
   artist
   (element-after type CD-list)) )

;; Return the elements up to and including selector in a-list. 
(define (items-before a-list selector)
  (subseq 
   a-list
   0
   (position selector a-list)) )

;; Return the elements up to and including selector in a-list. 
(define (items-before a-list selector)
  (subseq 
   a-list
   0
   (+ (position selector a-list) 1)) )

;; Return the elements following the artist selector and her CDs. 
(define (items-after a-list selector)
  (subseq 
   a-list
   (+ (position selector a-list) 2)) )

;; Return a-list with element replacing the item after selector. 
(define (new-element a-list element selector)
  (append 
   (items-before a-list selector)
   (list element)
   (items-after a-list selector)) )

;; Return a new CD-list with a new CD added for artist in
;; category. 
(define (add-new-CD CD category artist CD-list)
  (new-element
   CD-list 
   (new-element
    (element-after category CD-list)
    (cons 
     CD
     (artist-CD-list category artist CD-list) )
    artist)
   category) )
