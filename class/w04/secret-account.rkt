#lang racket

;; wrapper to avoid changing internal definitions
(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))

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
              ((eq? msg 'balance) balance)
              (else (lambda args (display "invalid request"))))))))


(define make-secret-account
  (lambda (initial password)
    (let ((account (make-account initial))
          (validated? #f))
      (define unlock
        (lambda (pin)
          (cond ((eq? pin password) (set! validated? #t))
                (else (lambda args (display "invalid pin"))))))
      (define lock
        (lambda ()
          (set! validated? #f)))
      (lambda (msg)
        (cond ((eq? msg 'unlock) unlock)
              ((eq? msg 'lock) lock)
              (validated? (account msg))
              (else (lambda args (display "account locked"))))))))

(define acct (make-secret-account 10 '12345))
(ask acct 'unlock '12345)
(ask acct 'balance)
(ask acct 'withdraw 5)
(ask acct 'deposit 100)
(ask acct 'lock)
;;(ask acct 'withdraw 5000)

;; consider passing pin to method, avoiding explicit lock/unlock