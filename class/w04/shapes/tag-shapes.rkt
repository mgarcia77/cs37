#lang racket

(require "table.rkt")

(define attach-tag cons)
(define tag-type car)
(define contents cdr)

(define make-circle
  (lambda (r)
    (attach-tag 'circle r)))

(define make-square
  (lambda (s)
    (attach-tag 'square s)))


(put 'circle 'perimeter (lambda (shape) (* 2 pi (contents shape))))
(put 'circle 'area (lambda (shape) (* pi (contents shape) (contents shape))))
(put 'square 'perimeter (lambda (shape) (* 4 (contents shape))))
(put 'square 'area (lambda (shape) (* (contents shape) (contents shape))))

(define sq (make-square 3))
(define circ (make-circle 3))


(define operate
  (lambda (obj op)
    (let ((fn (get (tag-type obj) op)))
      (cond ((void? fn) (error "shape/operation not supported"))
            (else (fn obj))))))


(operate sq 'perimeter)
(operate circ 'area)


