#lang racket

(define make-counter
  (lambda (initial)
    (let ((original initial))
      (define inc
        (lambda ()
          (set! initial (+ initial 1))
          initial))
      (define dec
        (lambda ()
          (set! initial (- initial 1))
          initial))
      (define reset
        (lambda ()
          (set! initial original)
          initial))
      (lambda (msg)
        (cond ((eq? msg 'inc) inc)
              ((eq? msg 'dec) dec)
              ((eq? msg 'reset) reset) ; sets counter back to initial value...?
              (else (error "get off my lawn")))))))


(define ctr (make-counter 10))
((ctr 'inc))

