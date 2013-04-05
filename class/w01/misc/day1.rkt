#lang racket

; lambda notation
(define add1
  (lambda (x)
    (+ x 1)))

; shortcut notation
(define (add2 x)
  (+ x 2))

; anonymous function
(define seven ((lambda (x) (+ x 1)) 6))

; 3 parameters, returns smallest
(define smallest
  (lambda (x y z)
    (if (and (< x y) (< x z)) x (if (< y z) y z))))
     ; if x is smallest, return x
     ; else if y is smaller than z, return y
     ; else return z