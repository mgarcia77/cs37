#lang racket

(define make-account
  (lambda (initial)
    (let ((original initial))
      (define deposit
        (lambda (money)
          (set! initial (+ initial money))
          initial))
      (define withdraw
        (lambda (money)
          (if (> money initial)
              "insufficient funds" ; perhaps charge surcharge per amandine
              (begin (set! initial (- initial money)) initial))))
          
      (define balance
        (lambda ()
          initial))
      (lambda (msg)
        (cond ((eq? msg 'deposit) deposit)
              ((eq? msg 'withdraw) withdraw)
              ((eq? msg 'balance) balance)
              (else (error "get off my lawn")))))))

(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))


(define account (make-account 10))
((account 'deposit) 50)
(apply (account 'deposit) '(50))
(ask account 'deposit 50)
((account 'withdraw) 500)
((account 'withdraw) 10)
((account 'balance))


