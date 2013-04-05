#lang racket

(require rackunit)

; generic procedure to check if lst is sorted
(define sorted?
  (lambda (lst comparator)
    (cond ((or (null? lst) (null? (cdr lst))) #t)
          ((comparator (car lst) (cadr lst)) (sorted? (cdr lst) comparator))
          (else #f))))

(check-equal? (sorted? '(1 2 4 6) <=) #t)
(check-equal? (sorted? '() <) #t)
(check-equal? (sorted? '(5) =) #t)
(check-equal? (sorted? '(1 2 8 6) >=) #f)


; returns procedure that adds n
(define add-n
  (lambda (n)
    (lambda (k) (+ k n))))

((add-n 50) 100)
((add-n 5) 7)