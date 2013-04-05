#lang racket

;; generic function to apply a function to each elt in a list
;; our implementation of built-in map
(define adjust-each
  (lambda (ls fn)
    (cond ((null? ls) null)
          (else (cons (fn (car ls)) (adjust-each (cdr ls) fn))))))

;; doubles each element in list
(define dbl-each
  (lambda (ls)
    (adjust-each ls (lambda (n) (* 2 n)))))

(dbl-each '(1 2 3 4))  ;; '(2 4 6 8)

;; extracts second element from each sublist in list of lists
(define second-elt
  (lambda (ls)
    (adjust-each ls cadr)))

(second-elt '((a 4) (b 12) (c 53)))  ;; '(4 12 53)
(map cadr '((a 4) (b 12) (c 53)))  ;; '(4 12 53)
