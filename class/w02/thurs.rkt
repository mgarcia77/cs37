#lang racket

(require rackunit)

(define sorted?
  (lambda (lst comparator)
    (cond ((or (null? lst) (null? (cdr lst))) #t)
          ((comparator (car lst) (cadr lst)) (sorted? (cdr lst)
                                                      comparator))
          (else #f))))

(check-equal? (sorted? '(1 2 4 6) <=) #t)
(check-equal? (sorted? '(1) <=) #t)
(check-equal? (sorted? '() <=) #t)
(check-equal? (sorted? '(1 2 8 6) <=) #f)
(check-equal? (sorted? '("apple" "banana" "pear") string<=?) #t)
(check-equal? (sorted? '(10 5 2 1) >=) #t)
(check-equal? (sorted? '(5 5 5 5 5) =) #t)
(check-equal? (sorted? '(5 5 4 5 5) =) #f)

#|
(define do-this
  (lambda (op a b)
    (op a b)))

(do-this + 1 2)
(do-this * 4 8)
|#


(define boring
  (lambda (k) (+ k 5)))
(boring 7)    ;12


(define add-n
  (lambda (n)
    (lambda (k) (+ k n))))

(define add-5 (add-n 5))
(add-5 7)
(add-5 100)

; some oddball list of functions in a list
(define fnlist (list (lambda (n) (+ n 1)) (add-n 2) cons))


(define range
  (lambda (n)
    (cond ((<= n 0) null)
          (else (cons (add-n n) (range (- n 1)))))))

(define functions (range 5))







