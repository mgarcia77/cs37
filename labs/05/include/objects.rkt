#lang racket

(provide ask make-object)

(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))

(define make-object
  (lambda ()
    (let ((super null)
          (this null))
      (define error-msg
        (lambda (msg) (error "object::bad message:" msg)))
      (define set-this!
        (lambda (obj)
          (set! this obj)))
      (define dispatch 
        (lambda (msg)
          (cond ((eq? msg 'set-this!) set-this!) 
                (else (ask this 'error-msg msg)))))
      (ask dispatch 'set-this! dispatch)
      this)))