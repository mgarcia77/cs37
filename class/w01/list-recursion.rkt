#lang racket

(require rackunit)

; a recursive function that determines the length of a list
(define my-length
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ 1 (my-length (cdr lst)))))))


(check-equal? (my-length '(1 2 3 4)) 4)
(check-equal? (my-length '(1)) 1)
(check-equal? (my-length '(1 (2 3))) 2) ; be sure you know why this is 2
(check-equal? (my-length '()) 0)

; A recursive function that sums the elements of a list.  Does not
; error check to be sure elements are numbers, though that is
; possible.
(define sum
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ (car lst) (sum (cdr lst)))))))

(check-equal? (sum '(1 2 3 4)) 10)
(check-equal? (sum '(1)) 1)
(check-equal? (sum '()) 0)

; A recursive function that multiplies the elements of a list.
(define product
  (lambda (lst)
    (cond ((null? lst) 1)
          (else (* (car lst) (product (cdr lst)))))))

(check-equal? (product '(1 2 3 4)) 24)
(check-equal? (product '(1)) 1)
(check-equal? (product '()) 1)

