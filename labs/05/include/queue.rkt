#lang racket

(require "objects.rkt")

(provide make-queue)

(define make-queue-node
  (lambda (val)
    (let ((fwd null)
          (super (make-object))
          (this null))
      (define set-forward! (lambda (ptr) (set! fwd ptr)))
      (define set-value!
        (lambda (new-val)
          (set! val new-val)
          (ask this 'view)))
      (define forward (lambda () fwd))
      (define value (lambda () val))
      (define view
        (lambda ()
          (list (ask this 'value)
                (ask this 'forward))))
      (define error-msg
        (lambda (msg) (error "queue-node::bad message:" msg)))
      (define set-this!
        (lambda (obj)
          (set! this obj)
          (ask super 'set-this! obj)))
      (define dispatch 
        (lambda (msg)
          (cond ((eq? msg 'set-forward!) set-forward!)
                ((eq? msg 'set-value!) set-value!)
                ((eq? msg 'forward) forward)
                ((eq? msg 'value) value)
                ((eq? msg 'view) view)
                ((eq? msg 'error-msg) error-msg)
                ((eq? msg 'set-this!) set-this!) 
                (else (super msg)))))
      (ask dispatch 'set-this! dispatch)
      this)))

(define make-queue
  (lambda ()
    (let ((head null)
          (tail null)
          (super (make-object))
          (this null))
      (define enqueue ;add an item to the end of the queue
        (lambda (value)
          (let ((new-tail (make-queue-node value)))
            (ask new-tail 'set-forward! null)
            (cond ((null? head)
                   (set! head new-tail))
                  (else
                   (ask tail 'set-forward! new-tail)))
            (set! tail new-tail))))
      (define dequeue ;remove (and return) the first item in the queue
        (lambda ()
          (let ((return-value null))
            (cond ((null? head) (error "queue::nothing to dequeue"))
                  (else
                   (set! return-value (ask head 'value))
                   (set! head (ask head 'forward))))
            return-value)))
      (define view    ;views the items in the queue as a list
        (lambda ()
          (define view-helper
            (lambda (ptr)
              (cond ((null? ptr) null)
                    (else (cons (ask ptr 'value)
                                (view-helper (ask ptr 'forward)))))))
          (view-helper head)))
      (define empty?
        (lambda ()
          (null? head)))
      (define error-msg
        (lambda (msg) 
          (error "queue::bad message:" msg)))     
      (define set-this!
        (lambda (obj)
          (set! this obj)
          (ask super 'set-this! obj)))      
      (define dispatch
        (lambda (msg)
          (cond ((eq? msg 'enqueue) enqueue)
                ((eq? msg 'dequeue) dequeue)
                ((eq? msg 'view) view)
                ((eq? msg 'empty?) empty?)
                ((eq? msg 'error-msg) error-msg)
                ((eq? msg 'set-this!) set-this!)
                (else (super msg)))))
      (ask dispatch 'set-this! dispatch)
      this)))
