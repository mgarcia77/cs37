#lang racket

(define make-obj
  (lambda ()
    (lambda (msg)
      (cond ((eq? msg 'type) 'object)
            (else (error "not supported"))))))

(define make-rec
  (lambda (side1 side2)
    (let ((super (make-obj)))
    (lambda (msg)
      (cond ((eq? msg 'perimeter) (+ side1 side1 side2 side2))
            ((eq? msg 'area) (* side1 side2))
            ((eq? msg 'type) 'rectangle)
            (else (super msg)))))))

(define make-square
  (lambda (side)
    (let ((super (make-rec side side)))
    (lambda (msg)
      (cond ((eq? msg 'type) 'square)
            (else (super msg)))))))

(define make-circle
  (lambda (radius)
    (let ((super (make-obj)))
    (lambda (msg)
      (cond ((eq? msg 'perimeter) (* 2 pi radius))
            ((eq? msg 'area) (* pi radius radius))
            ((eq? msg 'type) 'circle)
            (else (super msg)))))))


(define sq (make-square 4))
(define circ (make-circle 3))