#lang racket

;;; number-rep.rkt

;;; A data abstraction for arithmetic.  The base of this abstraction
;;; relies on us being able to describe zero, is-zero?, inc and dec.
;;; We can then write definitions of add, sub, mul, div, rem, exp,
;;; greater-than?, less-than? and equal-to? which rely only on these
;;; four primitives.

;;; We assume all numbers are greater than or equal to zero.

;;;---- integer arithmetic: Level 1 (arithmetic primitives) ----

(provide zero is-zero? inc dec)

(define zero 0)

(define is-zero?
  (lambda (n) (= n 0)))

(define inc
  (lambda (n) (+ n 1)))

(define dec
  (lambda (n) (- n 1)))

