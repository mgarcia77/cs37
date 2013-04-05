#lang racket

;;; list-rep.rkt

;;; A data abstraction for arithmetic.  

;;; list representation:
;;; 0 : ()
;;; 1 : (() ())
;;; 2 : (() () ())

;;; We assume all numbers are greater than or equal to zero.

;;;---- integer arithmetic: Level 1 (arithmetic primitives) ----

(provide zero is-zero? inc dec)

(define zero null)

(define is-zero?
  null?)

(define inc
  (lambda (n)
    (cons null n)))

(define dec
  (lambda (n)
    (cdr n)))

