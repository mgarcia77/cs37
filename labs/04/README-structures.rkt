#lang racket

;; Structures
;; This file is for informational purposes only.  There
;; is no lab assignment contained in this file.

;; In C, you could create a Fraction structure as follows:

#|
  
  typedef struct {
    int numerator;
    int denominator;
  } Fraction;

|#

;; The Racket equivalent is:

(define-struct fraction (numerator denominator))

;; When you define a structure, Racket creates a number
;; of helper functions for you.  For example, to make
;; a new fraction, you would use the function "make-fraction":

(define one-half (make-fraction 1 2))

;; To extract the numerator and denominator:

(fraction-numerator one-half)   ; => 1
(fraction-denominator one-half) ; => 2

;; In addition, Racket creates a function to test
;; whether or not something is a fraction structure:

(fraction? one-half) ; => #t

;; If you go back and look at the rgb structure used
;; in the graphics.rkt file, you will have a better understanding
;; of how colors are implemented.

;; For further information, see "define-struct" in the 
;; Racket documetation.

