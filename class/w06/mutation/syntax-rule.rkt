#lang racket

#|
;; only swaps local a and b (pass by value)
(define swap
  (lambda (a b)
    (let ((temp 0))
      (set! temp a)
      (set! a b)
      (set! b temp))))
|#

;; like #define in C: replacing code when called
(define-syntax-rule (swap x y)
  (let ((temp a))
    (set! a b)
    (set! b temp)))
     
(define a 3)
(define b 5)

(display a)
(display b)

(swap a b)

(display a)
(display b)