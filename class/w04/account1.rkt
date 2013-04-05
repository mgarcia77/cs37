#lang racket

(define make-account
  (lambda (init)
    (let ((orig init))
      (define deposit
        (lambda (x)
          (set! init (+ init x))
          init))
      (define withdraw
        (lambda (x)
          (if (> x init) "insufficient funds"
              (begin (set! init (- init x))
                     init))))
      (define balance
        (lambda ()
          init))
      (lambda (msg)
        (cond ((eq? msg 'deposit) deposit)
              ((eq? msg 'withdraw) withdraw)
              ((eq? msg 'balance) (balance))
              (else (error "invalid")))))))

;; wrapper to avoid changing internal definitions
(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))

(define account (make-account 10))

;((account 'deposit) 50)
(ask account 'deposit 50)
((account 'withdraw) 500)
((account 'withdraw) 10)
(account 'balance)