;; Code from Chapter 7 of Exploring Computer Science with Scheme
;; by Oliver Grillmeyer
;; Copyright 1998 Springer-Verlag

;; Encode the symbol letter.
(define (english-letter-to-code letter match-list)
  (let ( (letter-pair (first match-list)) )
    (if (equal? letter (first letter-pair))
        (second letter-pair)
        (english-letter-to-code letter (rest match-list)))) )

;; Encode letter-list.
(define (english-to-code letter-list match-list)
  (if (null? letter-list)
      '()
      (cons (english-letter-to-code (first letter-list) match-list)
            (english-to-code (rest letter-list) match-list))) )

;; Encode the symbol letter.
(define (english-letter-to-code letter match-list)
  (second (member letter match-list)) )

;; Like assoc but returns the first pair whose cdr matches elt.
(define (rassoc elt assoc-list)
  (cond ((null? assoc-list) #f)
        ((equal? (cdar assoc-list) elt) (car assoc-list))
        (else (rassoc elt (cdr assoc-list)))) )

;; Encode the symbol letter.
(define (english-letter-to-code letter match-list)
  (second (assoc letter match-list)) )

;; Decode the symbol letter.
(define (code-to-english-letter letter match-list)
  (car (rassoc (list letter) match-list)) )

;; Encode the symbol letter from an association list of dotted lists.
(define (english-letter-to-code letter match-list)
  (cdr (assoc letter match-list)) )

;; Decode the symbol letter from an association list of dotted lists.
(define (code-to-english-letter letter match-list)
  (car (rassoc letter match-list)) )

;; Encode the symbol letter.
(define (english-letter-to-code letter match-list)
  (if (equal? letter (first match-list))
      (second match-list)
      (english-letter-to-code letter (cddr match-list))) )

;; Return set with item added unless it already exists in set.
(define (adjoin item set)
  (if (member item set)
      set
      (cons item set)) )

;; Return the set of items in either set1 or set2.
(define (union set1 set2)
  (cond ((null? set1)
         set2)
        ((member (car set1) set2)
         (union (cdr set1) set2))
        (else
         (cons (car set1) (union (cdr set1) set2)))) )

;; Return the set of items in both set1 and set2.
(define (intersection set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((member (car set1) set2)
         (cons (car set1) (intersection (cdr set1) set2)))
        (else
         (intersection (cdr set1) set2))) )

;; Return the set of items in set1 but not in set2.
(define (set-difference set1 set2)
  (cond ((null? set2)
         set1)
        ((null? set1)
         '())
        ((member (car set1) set2)
         (set-difference (cdr set1) set2))
        (else
         (cons (car set1) (set-difference (cdr set1) set2)))) )

;; Return #t if all elements in set1 are also in set2, #f otherwise.
(define (subset? set1 set2)
  (cond ((null? set1)
         #t)
        ((null? set2)
         #f)
        (else
         (and (member (car set1) set2)
              (subset? (cdr set1) set2)))) )

(define places-i-have-been
  '(turkey belize thailand indonesia india))

(define places-brett-has-been
  '(south-dakota thailand))

(define places-lisa-has-been
  '(yugoslavia thailand belize turkey india))

;; Works only for binary trees

;; Use depth-first search to find item in tree.
(define (depth-first-search item tree)
  (cond ((null? tree) #f)                               ; empty tree
        ((atom? tree) (equal? item tree))               ; leaf
        ((equal? item (first tree)) #t)                 ; test root
        ((depth-first-search item (second tree)) #t)    ; test left side
        (else
         (depth-first-search item (third tree)))) )  ; test right side

;; Return the root of tree.
(define (root tree)
  (first tree))

;; Return the left subtree of tree.
(define (left-side tree)
  (second tree))

;; Return the right subtree of tree.
(define (right-side tree)
  (third tree))

;; Use depth-first search to find item in tree.
(define (depth-first-search item tree)
  (cond ((null? tree) #f)                    ; empty tree
        ((atom? tree) (equal? item tree))    ; leaf
        ((equal? item (root tree)) #t)       ; test root
        (else                                ; test left and right sides
         (or (depth-first-search item (left-side tree))
             (depth-first-search item (right-side tree))))) )

;; Use breadth-first search to find item in search-list (a list of trees).
(define (breadth-first-search item search-list)
  (let ( (current-tree (first search-list)) )
    (cond ((null? search-list) #f)
          ((equal? item (root current-tree)) #t)
          (else
           (breadth-first-search
            item
            (append
             (rest search-list)
             (list
              (left-side current-tree)
              (right-side current-tree))))))) )

;; Use breadth-first search to find item in search-list (a list of trees).
(define (breadth-first-search item search-list)
  (let ( (current-tree (first search-list)) )
    (cond ((null? search-list) #f)
          ((atom? current-tree)
           (or (equal? item current-tree)
               (breadth-first-search item (rest search-list))) )
          ((equal? item (root current-tree)) #t)
          (else
           (breadth-first-search
            item
            (append
             (rest search-list)
             (list
              (left-side current-tree)
              (right-side current-tree)))) ))) )

;; Only the let is written in the text.  Here it is added to the rest of the
;; function.
;; Use breadth-first search to find item in search-list (a list of trees).
(define (breadth-first-search item search-list)
  (let ( (current-tree
          (if (null? search-list)
              '()
              (first search-list))) )
    (cond ((null? search-list) #f)
          ((atom? current-tree)
           (or (equal? item current-tree)
               (breadth-first-search item (rest search-list))) )
          ((equal? item (root current-tree)) #t)
          (else
           (breadth-first-search
            item
            (append
             (rest search-list)
             (list
              (left-side current-tree)
              (right-side current-tree)))) ))) )

(define (depth-first-search-2 item tree)
  (cond ((null? tree) #f)
        ((atom? tree) (equal? item tree))
        (else
         (or (equal? item (root tree))
             (depth-first-search-2 item (left-side tree))
             (depth-first-search-2 item (right-side tree))))) )

(define (depth-first-search-3 item tree)
  (or (not (null? tree))
      (equal? item tree)
      (equal? item (root tree))
      (depth-first-search-3 item (left-side tree))
      (depth-first-search-3 item (right-side tree))) )

(define (who-knows tree)
  (cond ((null? tree) 0)
        ((symbol? tree) 0)
        ((number? tree) tree)
        (else (max
               (who-knows (left-side tree))
               (who-knows (right-side tree))))) )

(define (is-expression tree)
  (cond ((null? tree) #f)
        ((atom? tree) #t)
        (else (or
               (eq? (first tree) '(+ - * /))
               (number? (second tree))
               (is-expression (third tree))))) )

(define restaurants              ; (not written as a define in the text)
  '(fast-food (drive-in in-and-out-burger (ribs flints tommys))
              (ethnic new-delhi-junction la-vals)))

;; Return true if tree is a leaf (an atom).
(define (leaf? tree)
  (atom? tree))

;; Return list of restaurants according to properties in desired
;; and undesired lists.
(define (restaurant-advisor restaurant-tree desired undesired)
  (cond ((null? restaurant-tree) '())
        ((leaf? restaurant-tree) (list restaurant-tree))
        ((member (root restaurant-tree) desired)
         (restaurant-advisor (left-side restaurant-tree)
                             desired undesired))
        ((member (root restaurant-tree) undesired)
         (restaurant-advisor (right-side restaurant-tree)
                             desired undesired))
        (else
         (append
          (restaurant-advisor (left-side restaurant-tree)
                              desired undesired)
          (restaurant-advisor (right-side restaurant-tree)
                              desired undesired)))) )
