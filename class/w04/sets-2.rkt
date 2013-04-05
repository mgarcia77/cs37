#lang racket

; ****************************************************************************
; SET OPERATIONS
; (element? item set)      determines if the item is a member of the set
; (adjoin item set)        adds the item to the set to create a new set
; (intersection set1 set2) returns a set which contains only the items 
;			   that are in both set1 and set2
; (union set1 set2)        returns a set which contains all the items
;                          that are in either set1 or set2
; ****************************************************************************

; OPTION 2: Unordered list with duplicates

; element? and intersection are the same as OPTION 1, 
; but adjoin and union are rewritten.

(define set1 '(1 4 1 4 7 3 2))
(define set2 '(1 9 9 8 4))

(define element?
  (lambda (item set)
    (cond
     ((null? set) #f)
     ((= (car set) item) #t)
     (else (element? item (cdr set))))))

; You complete adjoin as an O(1) function
(define adjoin
  (lambda (item set)
    (cons item set)))

(define intersection
  (lambda (s1 s2)
    (cond
     ((or (null? s1) (null? s2)) '())
     ((element? (car s1) s2)
      (adjoin (car s1) (intersection (cdr s1) s2)))
     (else
      (intersection (cdr s1) s2)))))                                  

; You complete union as an O(n) function
(define union
  (lambda (s1 s2)
    (append s1 s2)))