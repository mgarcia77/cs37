#lang racket

(define find-smallest
  (lambda (a b c)
    (if (< a b)
        (if (< a c) a c) 
        (if (< b c) b c))))

; A solution using 'and':

(define find-smallest-and
  (lambda (a b c)
    (if (and (< a c) (< a b))  ; is a the smallest?
        a                      ; answer is a
        (if (< b c) b c))))    ; answer is smaller of b and c 

; A solution using 'cond':

(define find-smallest-cond
  (lambda (x y z) 
    (cond 
      ((and (<= x y) (<= x z)) x) ;is x smallest? 
      ((and (<= y x) (<= y z)) y) ;is y smallest? 
      (else z))))                 ;z must be smallest 


(provide find-smallest)