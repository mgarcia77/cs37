#lang racket

;; alarm-clock (parent) and clock-radio (child) objects
;; - adds a new "object" using (make-object)
;; - adds a "super" and "this" pointer, and a set-this! message
;; - adds dynamic dispatch for error messages

(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))

; Object is the top-most object and has no
; parent.  All other objects inherit from object or
; another already created object.
(define make-object
  (lambda ()
    (let ((super null) ; pointer to (non-existent) parent
          (this null)) ; pointer to this object
      (define set-this! ; see description in alarm-clock
        (lambda (obj)
          (set! this obj)))
      (define error-msg ; the error-handling function
        (lambda (msg)
          (error "object::bad message:" msg)))
      (define dispatch
        (lambda (msg)
          (cond ((eq? msg 'set-this!) set-this!)
                ; we need to handle calls to our error-handler
                ((eq? msg 'error-msg) error-msg) 
                (else (ask this 'error-msg msg)))))
      (ask dispatch 'set-this! dispatch) ; setup the "this" pointer
      this))) ; return the "this" pointer (or the "dispatch" pointer)

(define make-alarm-clock
  (lambda ()
    (let ((current-time 0)
          (alarm-time 0)
          (alarm-on? #f)
          (super (make-object))  ;pointer to parent object
          (this null))           ;pointer to this object
      (define status
        (lambda ()
          (list 'time current-time 
                'alarm (if alarm-on? alarm-time 'off))))
      (define reset
        (lambda ()
          (set! current-time 0)
          (set! alarm-time 0)
          (set! alarm-on? #f)
          (ask this 'status)))
      (define set-time
        (lambda (new-time)
          (set! current-time new-time)
          (ask this 'status)))
      (define tick
        (lambda ()
          (set! current-time (add1 current-time))
          (if (and alarm-on? (>= current-time alarm-time))
              (cons 'rinnnnnng (ask this 'status))
              (ask this 'status))))
      (define set-alarm
        (lambda (new-time)
          (set! alarm-time new-time)
          (set! alarm-on? #t)
          (ask this 'status)))
      (define alarm-off
        (lambda ()
          (set! alarm-on? #f)
          (ask this 'status)))
      (define set-this!                ;this function is called when
        (lambda (obj)                  ;the object is created:
          (set! this obj)              ;sets the this pointer and
          (ask super 'set-this! obj))) ;tells its parent to do the same
      (define error-msg 
        (lambda (msg) ; error-handler for alarm clock
          (error "alarm-clock::invalid message:" msg)))
      (define dispatch
        (lambda (msg)
          (cond
            ((eq? msg 'status) status)
            ((eq? msg 'reset) reset)
            ((eq? msg 'set-time) set-time)
            ((eq? msg 'tick) tick)
            ((eq? msg 'set-alarm) set-alarm)
            ((eq? msg 'alarm-off) alarm-off)
            ((eq? msg 'set-this!) set-this!)
            ; we need to handle calls to our error-handler
            ((eq? msg 'error-msg) error-msg) 
            (else (super msg)))))
      (ask dispatch 'set-this! dispatch) ;setup the "this" pointer
      this))) ; return the "this" pointer (or the "dispatch" pointer)

(display "Alarm Clock")
(define myclock (make-alarm-clock))
(ask myclock 'tick)
(ask myclock 'tick)
(ask myclock 'set-alarm 4)
(ask myclock 'tick)
(ask myclock 'tick)
(ask myclock 'tick)
(ask myclock 'alarm-off)
(ask myclock 'tick)
(ask myclock 'reset)
;(ask myclock 'synchronize)  ; an unhandled message


;;; The clock radio extends the basic alarm clock and
;;; inherits its functionality from the basic clock.

(define make-clock-radio
  (lambda ()
    (let ((band 'FM)
          (FM-frequency 88.5)
          (AM-frequency 1620)
          (super (make-alarm-clock)) ;pointer to parent object
          (this null))               ;pointer to this object
      (define status
        (lambda ()
          (append (list 'band band 'frequency 
                        (if (eq? band 'AM)
                            AM-frequency
                            FM-frequency))
                  (ask super 'status))))
      (define change-frequency
        (lambda (which-band new-frequency)
          (if (eq? which-band 'FM)
              (begin
                (set! band 'FM)
                (set! FM-frequency new-frequency))
              (begin
                (set! band 'AM)
                (set! AM-frequency new-frequency)))
          (ask this 'status)))
      (define set-this!      ;see description in alarm-clock
        (lambda (obj)
          (set! this obj)
          (ask super 'set-this! obj)))
      (define error-msg      ;error handler for clock-radio
        (lambda (msg)
          (error "clock-radio::invalid message:" msg)))
      (define dispatch
        (lambda (msg)
          (cond
            ((eq? msg 'status) status)
            ((eq? msg 'change-frequency) change-frequency)
            ((eq? msg 'set-this!) set-this!)
            ; we need to handle calls to our error-handler
            ((eq? msg 'error-msg) error-msg) 
            (else (super msg)))))
      (ask dispatch 'set-this! dispatch) ; setup the "this" pointer
      this))) ; return the "this" pointer (or the "dispatch" pointer)

(display "Clock Radio")
(define myclockradio (make-clock-radio))
(ask myclockradio 'change-frequency 'am 1010)
(ask myclockradio 'tick)
(ask myclockradio 'change-frequency 'fm 90.9)
(ask myclockradio 'tick)
(ask myclockradio 'status)
;(ask myclockradio 'play-mp3)

