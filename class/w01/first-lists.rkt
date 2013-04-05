#lang racket

; for unit testing using check-equal? and check-exn
(require rackunit)


; return a new list equal to the first element of each of three lists
(define first-lists
  (lambda (lst1 lst2 lst3)
    (if (or (null? lst1) (null? lst2) (null? lst3))
        (error "first-lists: each list must be non-empty")
        (list (car lst1) (car lst2) (car lst3)))
    )
  )

; an alternate version using cons (no error checking here)
(define first-lists-cons
  (lambda (lst1 lst2 lst3)
    (cons (car lst1) (cons (car lst2) (cons (car lst3) null)))
    )
  )


; check to be sure this function works with a few tests
(check-equal? (first-lists '(1 2 3) '(4 5 6) '(7 8 9)) '(1 4 7))
(check-equal? (first-lists '(1) '(4 5 6 7 8) '(9)) '(1 4 9))
(check-equal? (first-lists '(a b c) '(d e f) '(g h i)) '(a d g))

; check to be sure it properly fails when an empty list is provided
; the syntax is:
;
; (check-exn exn:fail? (lambda () [-what-you-want-to-check-]))
;

(check-exn exn:fail? (lambda () (first-lists '(1 2) '(3 4) '())))
