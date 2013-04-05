#lang racket

(require "objects.rkt")

(provide make-wire)

(define make-wire
  (lambda ()
    (let ((signal-value 0)
          (action-procedures '())
          (this null)
          (super (make-object)))
      (define call-each
        (lambda (procedures)
          (cond ((null? procedures) (void)) ;done
                (else
                 ((car procedures))
                 (call-each (cdr procedures)))))) 
      (define set-my-signal!
        (lambda (new-value)
          (cond ((= signal-value new-value) (void))
                (else 
                 (set! signal-value new-value)
                 (call-each action-procedures)))))
      (define get-signal
        (lambda () signal-value))
      (define accept-action-procedure! 
        (lambda (proc)
          (set! action-procedures (cons proc action-procedures))
          (proc)))
      (define error-msg
        (lambda (msg) (error "make-wire::bad message:" msg)))
      (define set-this!
        (lambda (obj)
          (set! this obj)
          (ask super 'set-this! obj)))
      (define dispatch
        (lambda (msg)
          (cond ((eq? msg 'get-signal) get-signal)
                ((eq? msg 'set-signal!) set-my-signal!)
                ((eq? msg 'add-action!) accept-action-procedure!)
                ((eq? msg 'error-msg) error-msg)
                ((eq? msg 'set-this!) set-this!)
                (else (super msg)))))
      (ask dispatch 'set-this! dispatch)
      this)))
