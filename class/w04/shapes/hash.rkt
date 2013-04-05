#lang racket

(define table (make-hash))

(hash-set! table 'apple 5)
(hash-set! table 'banana 7)

(hash-ref table 'apple)
(hash-ref table 'cookie 0)  ;; 0 is default case if key not found