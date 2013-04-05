#lang racket

(require "objects.rkt")
(require "queue.rkt")

(provide make-agenda)

;; Since a time segment is just a queue with a time added on to it,
;; we can create a time segment as an object which inherits from a
;; queue where we only have to create the additional functionality
;; for the time.  

;; The complete implementation of the time-segment object is below:

(define make-time-segment
  (lambda (time)                ; The time associated with the segment
    (let ((this null)
          (super (make-queue))) ; Inherit from make-queue
      (define segment-time      
        (lambda () ; Get the segment time
          time)) 
      (define error-msg
        (lambda (msg) (error "time-segment::bad message:" msg)))
      (define set-this!
        (lambda (obj)
          (set! this obj)
          (ask super 'set-this! obj)))
      (define dispatch 
        (lambda (msg)
          (cond ((eq? msg 'segment-time) segment-time)
                ((eq? msg 'set-this!) set-this!)
                ((eq? msg 'error-msg) error-msg)
                (else (super msg))))) 
      (ask dispatch 'set-this! dispatch)
      this)))



;; The complete implementation of the agenda is below.  You should
;; skim it for now, but it may be helpful to read through it in 
;; detail at a later time.

(define make-agenda
  (lambda ()
    (let ((current-time 0) ; Initialize time to 0
          (agenda null)    ; with no 'todo' items
          (this null)
          (super (make-object)))
      
      ; Helper function, not called directly
      ; as a result of a message.  
      (define insert-into-agenda-between!
        (lambda (seg-before seg-insert seg-after)
          (set-mcdr! seg-before seg-insert)
          (set-mcdr! seg-insert seg-after)))

      ; Helper function
      (define belongs-now?       ; Does the item we are inserting
        (lambda (seg-time seg-x) ; occur in this time segment?
          (= seg-time     
             (ask seg-x 'segment-time))))
      

      ; Helper function
      (define belongs-before?    ; Does the item we are inserting
        (lambda (seg-time seg-x) ; occur in an earlier time segment?
          (< seg-time
             (ask seg-x 'segment-time))))
      
      ; Helper function
      (define insert-in-order!
        (lambda (time item cur-agenda)
          (cond 
            ; If the item belongs in the cur-agenda time segment,
            ; add the item to the cur-agenda queue.
            ((belongs-now? time (mcar cur-agenda))
             (ask (mcar cur-agenda) 'enqueue item))
            ; If the item belongs before the cur-agenda time
            ; segment, insert a new time segment, "new-segment",
            ; to the agenda and add the item to the new-segment queue.
            ((or (null? (mcdr cur-agenda))
                 (belongs-before? time (mcar (mcdr cur-agenda))))
             (let ((new-segment (make-time-segment time)))
               (ask new-segment 'enqueue item)
               (set-mcdr! cur-agenda (mcons new-segment (mcdr cur-agenda)))))
            ; Otherwise the item belongs later in the queue
            (else
             (insert-in-order! time item (mcdr cur-agenda))))))
      
      (define insert!
        (lambda (time item)
          (cond ((< time current-time)
                 (error "agenda::cannot insert agenda item before current time"))
                ; Is this the first item in the agenda?
                ((or (null? agenda)
                     (belongs-before? time (mcar agenda)))
                 (let ((new-segment (make-time-segment time)))
                   (ask new-segment 'enqueue item)
                   (set! agenda (mcons new-segment agenda))))
                ; Otherwise, insert the item in its proper place
                (else
                 (insert-in-order! time item agenda)))))
      
      (define insert-after-delay!
        (lambda (delay item)
          (ask this 'insert! (+ current-time delay) item)))
      
      (define remove-next!
        (lambda ()
          (cond ((ask this 'empty?)
                 (error "agenda::nothing to remove"))
                (else
                 (let ((first-segment (mcar agenda)))
                   (let ((seg-time (ask first-segment 'segment-time))
                         (seg-item (ask first-segment 'dequeue)))
                     (if (ask first-segment 'empty?)
                         (set! agenda (mcdr agenda))
                         (void))
                     (set! current-time seg-time)
                     seg-item))))))
      
      (define propagate!
        (lambda ()
          (if (ask this 'empty?)
              (void) ;done
              (let ((first-item (ask this 'remove-next!)))
                (first-item) ; run the procedure first-item
                (ask this 'propagate!)))))
      
      (define get-time
        (lambda ()
          current-time))
      
      (define empty?
        (lambda ()
          (null? agenda)))
      
      (define error-msg
        (lambda (msg) (error "agenda::bad message:" msg)))
      
      (define set-this!
        (lambda (obj)
          (set! this obj)
          (ask super 'set-this! obj)))
      
      (define dispatch 
        (lambda (msg)
          (cond ((eq? msg 'insert!) insert!)
                ((eq? msg 'insert-after-delay!) insert-after-delay!)
                ((eq? msg 'remove-next!) remove-next!)
                ((eq? msg 'propagate!) propagate!)
                ((eq? msg 'get-time) get-time)
                ((eq? msg 'empty?) empty?)
                ((eq? msg 'set-this!) set-this!)
                ((eq? msg 'error-msg) error-msg)
                (else (super msg)))))
      (ask dispatch 'set-this! dispatch)
      this)))
