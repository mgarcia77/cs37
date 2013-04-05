#lang racket

(require rackunit)

;;;;;;;;;;;;;;;;;;;;
; LENGTH FUNCTIONS

; recursive solution
(define length-r
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ 1 (length (cdr lst)))))))

; wrapper for iterative process: init accumulator
(define length-i
  (lambda (lst)
    (length-helper lst 0)))

; tail recursive: racket doesn't need to hold previous function call
(define length-helper
  (lambda (lst answer)   ; answer is accumulator
    (cond ((null? lst) answer)
          (else (length-helper (cdr lst) (+ answer 1))))))   ; "increment" answer

(check-equal? (length-i '(1 2 3)) 3)

;;;;;;;;;;;;;;;;;
; SUM FUNCTIONS

; recursive sum of ints 0 to n
(define sum-r
  (lambda (n)
    (cond ((<= n 0) 0)
          (else (+ n (sum-r (- n 1)))))))

(check-equal? (sum-r 10) 55)

; wrapper for iterative sum
(define sum
  (lambda (n)
    (sum-i n 0)))

; adds all ints 0 to n (doesn't do neg ints)
(define sum-i
  (lambda (n total)
    (cond ((<= n 0) total)
          (else (sum-i (- n 1) (+ n total))))))

(check-equal? (sum 0) 0)
(check-equal? (sum 5) 15)


;;;;;;;;;;;;;;;;;;;;;
; BUILT N ELT LISTS
; build list of n elts, descending from n
(define descend-lst 
  (lambda (n)
    (cond ((<= n 0) null)
          (else (cons n (descend-lst (- n 1)))))))

; build list of n elts, ascending to n
; could append, but lists aren't mutable -> actually recopy list
(define ascend-lst
  (lambda (n)
    (cond 

