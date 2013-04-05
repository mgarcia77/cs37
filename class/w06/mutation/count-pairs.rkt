#lang racket

(require racket/mpair)

(define count-pairs*
  (lambda (mlst)
    (let ((cache (mlist)))  ;; use cache local to helper function to detect cycle
      
      (define count-helper
        (lambda (mlst)
          (cond ((null? mlst) 0)
                ((mmemq mlst cache) 0)
                ((mpair? (mcar mlst)) (set! cache (mcons mlst cache))
                                      (+ 1 (count-helper (mcar mlst))
                                         (count-helper (mcdr mlst))))
                (else (set! cache (mcons mlst cache))
                      (+ 1 (count-helper (mcdr mlst)))))))
      
      (count-helper mlst))))

(define mlst1 (mlist (mlist 1 2) 3 4))
(define mlst2 (mlist 1 2 3))

(count-pairs* mlst1)  ;; 5
(count-pairs* mlst2)  ;; 3

(define mlst3 (mlist 1 2 3))
(set-mcar! (mcdr mlst3) (mcdr (mcdr mlst3)))

(count-pairs* mlst3)  ;; should only be 3



(define display-cache
  (lambda (mlst)
    (cond ((null? mlst) (newline))
          (else (display (mcar mlst)) (newline) (display-cache (mcdr mlst))))))

(define cache (mlist mlst3 (mcdr mlst3) (mcar (mcdr mlst3))))

;; (mmemq (mcdr (mcdr mlst3)) cache)