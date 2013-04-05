#lang racket

(require rackunit)

#|
Here's an example of a block comment
|#

;; length, defined using a recursive process solution

(define length-r
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ 1 (length-r (cdr lst)))))))

(check-equal? (length-r '(1 2 3)) 3)

;;;;

(define length-i
  (lambda (lst answer)
    (cond ((null? lst) answer)
          (else (length-i (cdr lst) (+ 1 answer))))))

(define length
  (lambda (lst)
    (length-i lst 0)))

;; length, defined using an iterative process solution


(check-equal? (length '(1 2 3)) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/trace)  ; "import" the trace module

; sum the digits from 0 to n, using a recursive process

(define summer-r
  (lambda (n)
    (cond ((<= n 0) 0)
          (else (+ n (summer-r (- n 1)))))))

;(trace summer-r)      ; trace the summer-r function 


; sum the digits from 0 to n, using an iterative process

(define summer-i
  (lambda (n)
    (summer-i-helper n 0)))

(define summer-i-helper
  (lambda (n answer)
    (cond ((<= n 0) answer)
          (else (summer-i-helper (sub1 n) (+ answer n))))))


(check-equal? (summer-r 10) 55)
;(untrace summer-r)     ; untrace the summer-r function

(check-equal? (summer-r 30) (summer-i 30))

;;;;;;;;;;;;;;;;

;; create a list of numbers from 1 to n, recursive process

(define build-numlist-r
  (lambda (n)
    (cond ((<= n 0) null)
          (else (cons n (build-numlist-r (- n 1)))))))

(check-equal? (build-numlist-r 5) '(5 4 3 2 1))

;; create a list of numbers from 1 to n, iterative process

(define build-numlist-i
  (lambda (n)
    (numlist-helper n null)))

(define numlist-helper
  (lambda (n answer)
    (cond ((<= n 0) answer)
          (else (numlist-helper (- n 1) (cons n answer))))))

(check-equal? (build-numlist-i 5) '(1 2 3 4 5))
(check-equal? (build-numlist-i 0) '())

; identity function: return copy of list
(define identity
  (lambda (lst)
    (cond ((null? lst) null)
          (else (cons (car lst) (identity (cdr lst)))))))

(define identity-i
  (lambda (lst)
    (identity-helper lst '())))

(define identity-helper
  (lambda (lst result)
    (cond ((null? lst) result)
          (else (identity-helper (cdr lst) (cons (car lst) result))))))

(check-equal? (identity-i '(a b c)) '(c b a))
  
(define reverse-lst identity-i)  

; absolute value
(define abs-lst-r
  (lambda (lst)
    (cond ((null? lst) null)
          (else (cons (abs (car lst)) (abs-lst-r (cdr lst)))))))

(define abs-lst-i
  (lambda (lst)
    (define helper
      (lambda (lst result)
        (cond ((null? lst) (reverse-lst result))   ; can also reverse in abs-lst-i
              (else (helper (cdr lst) (cons (abs (car lst)) result))))))
    (helper lst '())))


;(abs-lst-i '(1 2 3))

(check-equal? (abs-lst-r '(1 2 3)) (abs-lst-i '(1 2 3)))
(check-equal? (abs-lst-r '(-1 -2 -3)) (abs-lst-i '(-1 -2 -3)))
(check-equal? (abs-lst-r '(0)) (abs-lst-i '(0)))
(check-equal? (abs-lst-r '()) (abs-lst-i '()))
(check-equal? (abs-lst-r '(1 -2 -3 4)) (abs-lst-i '(1 -2 -3 4)))
  
  

