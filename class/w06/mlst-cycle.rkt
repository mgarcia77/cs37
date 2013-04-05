#lang racket

(require racket/mpair)

(define x 5)
(set! x (+ x 1))

(define lst1 '(1 2 3))
(define lst2 '(4 5 6))
(define lst3 (append lst1 lst2))

(eq? (cdddr lst3) lst2)  ;; #t (eq if pointing to same memory)

(define lst4 '(1 2 3 4 5 6))
(eq? (cdddr lst4) lst2)  ;; #f

; (set! (car lst2) 8)  ;; must have var name for set!

(mcons 3 '())  ;; mutable cons pair

(define mlst1 (mlist 1 2 3))
(define mlst2 (mlist 4 5 6))
(define mlst3 (mappend mlst1 mlst2))

; (eq? (cdddr mlst3) mlst2)  ;; can't have car/cdr of mlst
(eq? (mcdr (mcdr (mcdr mlst3))) mlst2)  ;; #t (mcar/mcdr for mcons, no e.g. mcdddr)

;; can set-mcar! and set-mcdr! of mcons
(set-mcar! mlst2 8)  ;; also changes in mlst3


;; find last pair in mlist
(define last-pair
  (lambda (mlst)
    (cond ((null? mlst) (error "empty lst"))
          ((null? (mcdr mlst)) mlst)
          (else (last-pair (mcdr mlst))))))

(define append!
  (lambda (m1 m2)
    (set-mcdr! (last-pair m1) m2)))


(append! mlst2 mlst1)


;; can now build cycles in mlists
(define cycle (mlist 1 2 3))

(define make-cycle
  (lambda (mlst)
    (set-mcdr! (last-pair mlst) mlst)))

(make-cycle cycle)  ;; doesn't print in infinite loop but indicates cycle