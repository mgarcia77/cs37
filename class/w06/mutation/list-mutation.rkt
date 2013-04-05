#lang racket

(require racket/mpair)

(define last-pair
  (lambda (ls)
    (cond
      ((null? ls) (error "no last pair in" ls))
      ((null? (mcdr ls)) ls)
      (else (last-pair (mcdr ls))))))

; mutate list ls1 to point to ls2; return ls1
(define m-append!
  (lambda (ls1 ls2)                   
    (set-mcdr! (last-pair ls1) ls2)
    ls1))


(define mlst1 (mlist 'a 'b))
(define mlst2 (mlist 'c 'd))

(define mlst3a (mappend mlst1 mlst2)) ; append for mutable lists
(define mlst3b (m-append! mlst1 mlst2))

(set-mcar! mlst1 'j)
(set-mcar! mlst2 'k)

mlst3a  ; (a b k d)    ;; copy of mlst1 points to mlst2
mlst3b  ; (j b k d)    ;; mlst3b is mutated to contain mlst2


(define make-cycle
  (lambda (ls)
    (set-mcdr! (last-pair ls) ls)
    ls))


(define b (mlist 'a 'b 'c))
(define c (make-cycle b))

c ; #0=(mcons 'a (mcons 'b (mcons 'c #0#)))

