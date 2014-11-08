;; Code from Chapter 5 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

(define qualities '(cat (independent lazy sleepy)
                        dog (needy loyal) fish (wet slimy colorful) lion (dangerous)))

;; Return true if item is a symbol or a number, false otherwise.
(define (atom? item)
  (or (symbol? item) (number? item)) )

(define (month month-num)
  (list-ref '(January February March April May June July
                      August September October November December)
            month-num) )

(define scale              ; (not written as a define in the text)
  '(A A-sharp B C C-sharp D D-sharp E F F-sharp G G-sharp))

(define intervals          ; (not written as a define in the text)
  '(unison minor-second major-second minor-third major-third
           perfect-fourth diminished-fifth perfect-fifth augmented-fifth
           major-sixth minor-seventh major-seventh))

;; Return the musical interval between note1 and note2.
(define (interval note1 note2 scale-list interval-list)
  (let ( (distance (- (position note2 scale-list)
                      (position note1 scale-list))) )
    (if (positive? distance)
        (list-ref interval-list distance)
        (list-ref interval-list (- 12 distance)))) )

;; Return the musical interval between note1 and note2.
(define (interval note1 note2 scale-list interval-list)
  (let ( (distance (- (position note2 scale-list)
                      (position note1 scale-list))) )
    (if (negative? distance)
        (list-ref interval-list (+ 12 distance))
        (list-ref interval-list distance))) )

;; Return the note an interval above note.
(define (higher-note note interval scale-list interval-list)
  (let ( (half-steps (position interval interval-list))
         (note-position (position note scale-list)) )
    (list-ref scale-list
              (remainder (+ note-position half-steps) 12))) )

(define alt-scale-list     ; (not written as a define in the text)
  '(A A-sharp B C C-sharp D D-sharp E F F-sharp G G-sharp
      A A-sharp B C C-sharp D D-sharp E F F-sharp G G-sharp))

(define (interval-alt note1 note2 scale-list interval-list)
  (let ( (distance (- (position note2 (reverse scale-list))
                      (position note1 scale-list))) )
    (list-ref interval-list distance)) )

(define (higher-note-alt note interval scale-list interval-list)
  (let ( (half-steps (position interval interval-list))
         (note-position (position note scale-list)) )
    (list-ref scale-list (+ note-position half-steps))) )

;; Return the value of a poker hand.
(define (poker-value hand)
  (let ( (count-list (list
                      (count (first hand) hand)
                      (count (second hand) hand)
                      (count (third hand) hand)
                      (count (fourth hand) hand)
                      (count (fifth hand) hand))) )
    (cond
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
     (else 'nothing))) )
