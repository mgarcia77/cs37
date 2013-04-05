#lang racket

;;; File: binary-template.rkt

;;; binary representation of integers

;; The goal here is to represent numbers in binary.

;; To do this most easily, we will represent a binary
;; number as a list of 1's and 0's.

;; One natural way to do this is to represent numbers as
;; follows:
;;
;; 0 ==> (0)
;; 1 ==> (1)
;; 2 ==> (1 0)
;; 3 ==> (1 1)
;; 4 ==> (1 0 0)
;; 5 ==> (1 0 1)
;; 6 ==> (1 1 0)
;;
;; However, it is substantially easier to make 0 be the
;; empty list, instead of (0):
;;
;; 0 ==> ()
;; 1 ==> (1)
;; 2 ==> (1 0)
;; 3 ==> (1 1)
;; 4 ==> (1 0 0)
;; 5 ==> (1 0 1)
;; 6 ==> (1 1 0)
;;
;; Finally, it is more efficient to represent the binary
;; numbers as lists of 1's and 0's in REVERSE:
;;
;; 0 ==> ()
;; 1 ==> (1)
;; 2 ==> (0 1)
;; 3 ==> (1 1)
;; 4 ==> (0 0 1)
;; 5 ==> (1 0 1)
;; 6 ==> (0 1 1)
;;
;; Any of the above (or similar) representations that you
;; choose are acceptable.
;; 
;; Please show me that you have a working version of 
;; this before class ends.  

(define zero null)

(define is-zero? 
  null?)

(define inc
  (lambda (n)
    (cond ((is-zero? n) '(1))    ;; if n : 0, then (inc n) : '(1)
          ((= (car n) 0) (cons 1 (cdr n)))
          (else (cons 0 (inc (cdr n)))))))

(define dec
  (lambda (n)
    (cond ((null? (cdr n)) '())    ;; if n : (1), then (dec n) : '()
          ((= (car n) 1) (cons 0 (cdr n)))
          (else (cons 1 (dec (cdr n)))))))

(provide zero is-zero? inc dec)
