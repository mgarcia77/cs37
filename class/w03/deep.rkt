#lang racket

(define member?
  (lambda (elt lst)
    (cond ((null? lst) #f)
          ((equal? elt (car lst)) #t)
          (else (member? elt (cdr lst))))))

(member? 3 '(4 5 6 2 3 9))
(member? 3 '(4 5 6 2 7 9))

(define lst '(4 (5 6) (2 (3 9))))

(member? 3 '(4 (5 6) (2 (3 9))))



(define member*?
  (lambda (elt lst)
    (cond ((null? lst) #f)
          ((pair? (car lst)) 
           (or (member*? elt (car lst))
               (member*? elt (cdr lst))))
          ((equal? elt (car lst)) #t)
          (else (member*? elt (cdr lst))))))

(member*? 3 '(4 (((((3)))) 2) (8 2 1)))
(member*? 3 '(4 (5 6) (2 (3 9))))

(define sum
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ (car lst) (sum (cdr lst)))))))

(sum '(1 2 3 14 5))
;(sum '(1 2 (3 14) 5))

(define sum*
  (lambda (lst)
    (cond ((null? lst) 0)
          ((not (pair? lst)) lst)
          ((pair? (car lst)) 
           (+ (sum* (car lst))
              (sum* (cdr lst))))
          (else (+ (car lst) (sum* (cdr lst)))))))

(sum* '(1 2 (3 . 14) 5))
(sum* '(3 . 2))
