#lang racket

(define ask
  (lambda (obj msg . args)
    (apply (obj msg) args)))

(define make-alarm-clock
  (lambda ()
    (let ((current-time 0)
          (alarm-time 0)
          (alarm-on? #f))
      (define status
        (lambda ()
          (list 'time current-time 
                'alarm (if alarm-on? alarm-time 'off))))
      (define reset
        (lambda ()
          (set! current-time 0)
          (set! alarm-time 0)
          (set! alarm-on? #f)
          (status)))
      (define set-time
        (lambda (new-time)
          (set! current-time new-time)
          (status)))
      (define tick
        (lambda ()
          (set! current-time (add1 current-time))
          (if (and alarm-on? (>= current-time alarm-time))
              (cons 'rinnnnnng (status))
              (status))))
      (define set-alarm
        (lambda (new-time)
          (set! alarm-time new-time)
          (set! alarm-on? #t)
          (status)))
      (define alarm-off
        (lambda ()
          (set! alarm-on? #f)
          (status)))
      (define dispatch
        (lambda (msg)
          (cond
            ((eq? msg 'status) status)
            ((eq? msg 'reset) reset)
            ((eq? msg 'set-time) set-time)
            ((eq? msg 'tick) tick)
            ((eq? msg 'set-alarm) set-alarm)
            ((eq? msg 'alarm-off) alarm-off)
            ;((eq? msg 'snooze) snooze)  ;; handle this message
            (else (error "alarm-clock::invalid message:" msg)))))
      dispatch)))



