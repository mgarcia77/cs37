#lang racket

(require rackunit)

(define my-length
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ 1 (my-length (cdr lst)))))
    ))

(check-equal? (my-length '()) 0)
(check-equal? (my-length '(1)) 1)
(check-equal? (my-length '(1 2 3 4)) 4)

(define sum-elts
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ (car lst) (sum-elts (cdr lst)))))
    ))

(check-equal? (sum-elts '()) 0)
(check-equal? (sum-elts '(20)) 20)
(check-equal? (sum-elts '(1 2 3 4)) 10)

(define mult-elts
  (lambda (lst)
    (cond ((null? lst) 1)
          (else (* (car lst) (mult-elts (cdr lst)))))
    ))

(check-equal? (mult-elts '()) 1)
(check-equal? (mult-elts '(100)) 100)
(check-equal? (mult-elts '(1 2 3 4 5)) 120)