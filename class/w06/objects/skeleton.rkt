#lang racket

;; Skeleton file for creating objects with inheritance
;; and dynamic dispatch.

(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Object is the top-most object.  There is no need
; to modify this code.                                                 
(define make-object
  (lambda ()
    (let ((super null) 
          (this null)) 
      (define set-this! 
        (lambda (obj)
          (set! this obj)))
      (define error-msg 
        (lambda (msg)
          (error "object::bad message:" msg)))
      (define dispatch
        (lambda (msg)
          (cond ((eq? msg 'set-this!) set-this!)
                ((eq? msg 'error-msg) error-msg) 
                (else (ask this 'error-msg msg)))))
      (ask dispatch 'set-this! dispatch)
      this)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; An object that inherits only from Object.

(define make-parent-object
  (lambda ()
    (let ((local-variable-1 0) ; you may declare 0 or more
          (local-variable-2 0) ; local variables but you must
          (local-variable-3 0) ; always declare these:
          (super (make-object)); super: the pointer to my parent
          (this null))         ; this:  the pointer to myself
      (define msg-handler-1
        (lambda ()
          0))
      (define msg-handler-2    ; You can define any number of 
        (lambda ()             ; internal functions to help handle
          0))                  ; messages
      (define msg-handler-3
        (lambda (x y z)
          0))
      (define set-this!        ; This function must exist in every
        (lambda (obj)          ; new object exactly as shown here.
          (set! this obj)      ; (It is different in make-object)
          (ask super 'set-this! obj)))
      (define error-msg        ; Each new object must have an error
        (lambda (msg)          ; handler such as this one.
          (error "parent-object::invalid message:" msg)))
      (define dispatch
        (lambda (msg)
          (cond
            ((eq? msg 'msg-1) msg-handler-1) ; You can have any 
            ((eq? msg 'msg-2) msg-handler-2) ; number of acceptable
            ((eq? msg 'msg-3) msg-handler-3) ; messages

            ((eq? msg 'set-this!) set-this!) ; You must always handle
            ((eq? msg 'error-msg) error-msg) ; these two messages

            (else (super msg))))) ; Pass all unknown messages up

      (ask dispatch 'set-this! dispatch) ; setup the "this" pointer
      this))) ; return the "this" pointer which defines the object

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; An object that inherits from parent-object.  The only real difference
; is in the 7th line down where we specify what the parent object is 
; by saying "(super (make-parent-object))".

(define make-child-object
  (lambda ()
    (let ((local-variable-4 0) ; you may declare 0 or more
          (local-variable-5 0) ; local variables but you must
          (local-variable-6 0) ; always declare these:
          (super (make-parent-object)) ; super: parent pointer
          (this null))         ; this:  the pointer to myself
      (define msg-handler-1
        (lambda ()
          0))
      (define msg-handler-4    ; You can define any number of 
        (lambda ()             ; internal functions to help handle
          0))                  ; messages
      (define msg-handler-7
        (lambda (x y z)
          0))
      (define set-this!        ; This function must exist in every
        (lambda (obj)          ; new object exactly as shown here.
          (set! this obj)      ; (It is different in make-object)
          (ask super 'set-this! obj)))
      (define error-msg        ; Each new object must have an error
        (lambda (msg)          ; handler such as this one.
          (error "child-object::invalid message:" msg)))
      (define dispatch
        (lambda (msg)
          (cond
            ((eq? msg 'msg-1) msg-handler-1) ; You can have any 
            ((eq? msg 'msg-4) msg-handler-4) ; number of acceptable
            ((eq? msg 'msg-7) msg-handler-7) ; messages

            ((eq? msg 'set-this!) set-this!) ; You must always handle
            ((eq? msg 'error-msg) error-msg) ; these two messages

            (else (super msg))))) ; Pass all unknown messages up

      (ask dispatch 'set-this! dispatch) ; setup the "this" pointer
      this))) ; return the "this" pointer which defines the object

