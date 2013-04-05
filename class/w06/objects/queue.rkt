#lang racket

;;
;; A definition of nodes in a Queue
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The ask function

(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Object

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A defintion of a node in a queue

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
      (define set-this!
        (lambda (obj)
          (set! this obj)
          (ask super 'set-this! obj)))
      (define error-msg
        (lambda (msg) (error "queue-node::bad message:" msg)))
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

;(display "Queue Node 1\n")
;(define qn1 (make-queue-node 5))
;(ask qn1 'value)
;(ask qn1 'set-value! 10)
;;(ask qn1 'bad-message)  ;; error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A definition of a node in a doubly-ended queue.

(define make-deque-node
  (lambda (val)
    (let ((bwd null)
          (super (make-queue-node val))
          (this null))
      (define set-backward! (lambda (ptr) (set! bwd ptr)))
      (define backward (lambda () bwd))      
      (define view
        (lambda ()
          (list (ask this 'backward) 
                (ask this 'value)
                (ask this 'forward))))
      (define set-this!
        (lambda (obj)
          (set! this obj)
          (ask super 'set-this! obj)))
      (define error-msg
        (lambda (msg) 
          (error "deque-node::bad message:" msg)))
      (define dispatch 
        (lambda (msg)
          (cond ((eq? msg 'set-backward!) set-backward!)
                ((eq? msg 'backward) backward)
                ((eq? msg 'view) view)
                ((eq? msg 'error-msg) error-msg)
                ((eq? msg 'set-this!) set-this!)
                (else (super msg)))))
      (ask dispatch 'set-this! dispatch)
      this)))

;(display "Deque Node 1\n")
;(define dn1 (make-deque-node 5))
;(ask dn1 'value)
;(ask dn1 'view)
;(ask dn1 'set-value! 10) ;; Notice the view command prints the deque view
;(ask dn1 'bad-message)  ;; Notice the error message comes from deque-node

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-queue
  (lambda ()
    (let ((head null)
          (tail null)
          (size 0)
          (super (make-object))
          (this null))
      (define enqueue
        (lambda (value)
          (let ((new-tail (make-queue-node value)))
            (ask new-tail 'set-forward! null)
            (cond ((null? head)
                   (set! head new-tail))
                  (else
                   (ask tail 'set-forward! new-tail)))
            (set! tail new-tail)
            (set! size (+ size 1)))))
      (define dequeue
        (lambda ()
          (let ((return-value null))
            (cond ((null? head) (error "queue::nothing to dequeue"))
                  (else
                   (set! return-value (ask head 'value))
                   (set! head (ask head 'forward))
                   (set! size (- size 1))))
            return-value)))
      (define front
        (lambda ()
          (cond ((null? head) (error "queue::queue is empty"))
                (else (ask head 'value)))))
      (define view
        (lambda ()
          (define view-helper
            (lambda (ptr)
              (cond ((null? ptr) null)
                    (else (cons (ask ptr 'value)
                                (view-helper (ask ptr 'forward)))))))
          (view-helper head)))
      (define empty
        (lambda ()
          (set! head null)
          (set! tail null)
          (set! size 0)))
      (define length
        (lambda ()
          size))
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
                ((eq? msg 'front) front)
                ((eq? msg 'view) view)
                ((eq? msg 'empty) empty)
                ((eq? msg 'length) length)
                ((eq? msg 'error-msg) error-msg)
                ((eq? msg 'set-this!) set-this!)
                (else (super msg)))))
      (ask dispatch 'set-this! dispatch)
      this)))

                  
;(display "Queue 1\n")
;(define q1 (make-queue))
;(ask q1 'enqueue 5)
;(ask q1 'enqueue 10)
;(ask q1 'enqueue 15)
;(ask q1 'view)      ;(5 10 15)
;(ask q1 'length)    ;3
;
;(ask q1 'dequeue)   ;5
;(ask q1 'enqueue 8)
;(ask q1 'view)      ;(10 15 8)
;(ask q1 'dequeue)   ;10
;(ask q1 'dequeue)   ;15
;(ask q1 'dequeue)   ;8
;;(ask q1 'dequeue)  ;error - nothing to dequeue
