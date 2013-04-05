#lang racket

;; A simplistic version of a predicate that tests for primality.

(define prime?
  (lambda (n)
    (define prime-loop
      (lambda (n dvsr)
        (cond ((< n (* dvsr dvsr)) #t) ; stop when divisor exceeds sqrt n
              ((= 0 (remainder n dvsr)) #f) ; divisor divides into n evenly
              (else (prime-loop n (+ dvsr 2)))))) ; try again with dvsr+2
    (cond ((= 2 n) #t)  ; 2 is prime
          ((even? n) #f) ; all other evens are not
          ((< n 2) #f) ; nor are any numbers less than 2
          (else (prime-loop n 3))))) ; try dividing by 3


(provide prime?)
