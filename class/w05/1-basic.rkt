#lang racket

(define make-object-a
  (lambda (x y)
    (lambda (msg)
      (cond ((eq? msg 'first) x)
            ((eq? msg 'second) y)
            (else (error "oject-a::invalid message"))))))

(define a1 (make-object-a 100 200))
(define a2 (make-object-a "hello" "goodbye"))
(a1 'first)
(a1 'second)
(a2 'second)
;(a2 'third)


(define make-object-b
  (lambda (x y)
    (define change-x
      (lambda (val)
        (set! x val)))
    (define change-y
      (lambda (val)
        (set! y val)))
    (lambda (msg)
      (cond ((eq? msg 'change-first) change-x)
            ((eq? msg 'change-second) change-y)
            ((eq? msg 'first) x)
            ((eq? msg 'second) y)
            (else (error "go away"))))))

(define b1 (make-object-b 100 200))
(define b2 (make-object-b "hello" "goodbye"))



