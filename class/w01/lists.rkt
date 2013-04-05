#lang racket

(require rackunit)

; creates new list using the first element from each of three given lists
(define first-lists
  (lambda (lst1 lst2 lst3)
    (if (or (null? lst1) (null? lst2) (null? lst3)) 
        (error "first-lists: all lists must be non-empty")
        (list (car lst1) (car lst2) (car lst3)))))

(define first-lists-cons
  (lambda (lst1 lst2 lst3)
    (cons (car lst1) (cons (car lst2) (cons (car lst3) null)))))

;(check-equal? (first-lists '(1 2 3) '(4 5 6) '(7 8 9)) '(1 4 7))
;(check-exn exn:fail? (lambda () (first-lists '(1 2) '(3 4) '(5 6))))
(first-lists '(1 2 3) '(4 5 6) '(7 8 9)) ; '(1 4 7)
(first-lists-cons '(9 8 7) '(6 5 4) '(3 2 1)) ; '(9 6 3)

;(first-lists '() '() '())