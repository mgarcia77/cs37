#lang racket

(define add1-to-each
  (lambda (ls)
    (cond ((null? ls) null)
          (else (cons (+ 1 (car ls)) (add1-to-each (cdr ls)))))))

(add1-to-each '(1 3 5 6))


(define double-each
  (lambda (ls)
    (cond ((null? ls) null)
          (else (cons (* 2 (car ls)) (double-each (cdr ls)))))))

(double-each '(1 3 5 6))


(define adjust-each
  (lambda (ls fn)
    (cond ((null? ls) null)
          (else (cons (fn (car ls)) (adjust-each (cdr ls) fn))))))

(define dbl-each
  (lambda (ls)
    (adjust-each ls (lambda (n) (* 2 n)))))

(dbl-each '(1 3 5 6))

(define extract-second   ; this is the same as cadr
  (lambda (x) (car (cdr x))))

(define extractor
  (lambda (ls)
    (adjust-each ls cadr)))

(extractor '((a 3) (b 6) (c 21)))

(map add1 '(1 2 3 5))
(map cadr '((a 3) (b 6) (c 21)))
(map + '(1 3 5) '(10 20 30) '(200 400 600))
(map cons '(1 3 5) '(10 20 30))

;;;;