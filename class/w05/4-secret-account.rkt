#lang racket

(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))


(define make-account
  (lambda (initial)
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
              (else (error "get off my lawn"))))))

(define make-secret-account
  (lambda (initial password)
    (let ((account (make-account initial))
          (validated? #f))
      (define displayer
        (lambda (msg)
          (lambda args (display msg))))
      (define unlock
        (lambda (pin)
          (cond ((eq? pin password) (set! validated? #t))
                (else (set! validated? #f) (displayer "invalid password!")))))
      (define lock
        (lambda ()
          (set! validated? #f)))
      (lambda (msg)
        (cond ((eq? msg 'unlock) unlock)
              ((eq? msg 'lock) lock)
              (validated? (account msg))
              (else (displayer "account is locked!\n")))))))
      
(define acct1 (make-secret-account 100 '12345))
;(ask acct1 'balance)
;(ask acct1 'withdraw 50)
(ask acct1 'unlock '12345)
(ask acct1 'withdraw 50)
(ask acct1 'balance)
(ask acct1 'lock)
(ask acct1 'balance)
(ask acct1 'withdraw 50)

; To think about:
; Instead of explicit unlock and lock method, pass the pin to each of the methods
; (ask acct1 'balance '12345)
; (ask acct1 'withdraw 50 '12345)

