#lang racket

(require rackunit)


(define myexpt
  (lambda (a b)
    (display "YAY\n")
    (expt a b)))

(define f (lambda () (expt 2 500)))  ;; not calculated until f is called

#|
(define delay-expression
  (lambda (expr)
    (lambda () expr)))
|#

(define-syntax-rule (delay-expression expr)
  (lambda () expr))

(define force-expression
  (lambda (expr)
    (expr)))
  
(define g (delay-expression (myexpt 2 500)))

(force-expression g)  ;; same as (g)

(define x 12)
(define e2 (delay-expression (begin (printf "in e2, x = ~a\n" x) (+ x 5))))
(force-expression e2)

(set! x 13)
(force-expression e2)


(define-syntax-rule (newif condc thenc elsec)
  (if condc thenc elsec))