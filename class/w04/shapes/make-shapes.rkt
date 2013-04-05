#lang racket

(define attach-tag cons)
(define tag-type car)
(define contents cdr)

(define make-integer
  (lambda (n)
    (attach-tag 'integer n)))

(define make-rational
  (lambda (n d)
    (attach-tag 'rational (cons n d))))

(define make-square
  (lambda (s)
    (attach-tag 'square s)))

(define make-circle
  (lambda (r)
    (attach-tag 'circle r)))

(define make-eq-triangle
  (lambda (s)
    (attach-tag 'triangle s)))


(define area
  (lambda (shape)
    (cond ((eq? (tag-type shape) 'square)
           (* (contents shape) (contents shape)))
          ((eq? (tag-type shape) 'circle)
           (* pi (contents shape) (contents shape)))
          (else 
           (error "Not a supported shape")))))

(define perimeter
  (lambda (shape)
    (cond ((eq? (tag-type shape) 'square)
           (* (contents shape) 4))
          ((eq? (tag-type shape) 'circle)
           (* 2 pi (contents shape)))
          (else 
           (error "Not a supported shape")))))


(define circ (make-circle 5))
(define sq (make-square 5))