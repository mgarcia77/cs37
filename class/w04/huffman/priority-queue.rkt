#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An inefficient implementation of priority queues
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-pqueue pqueue-empty? pqueue-head pqueue-rest pqueue-data
         pqueue-insert list->pqueue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-pqueue
  (lambda (compare< . data)
    (let ((q (if (null? data) null (car data))))
      (list 'pqueue compare< q))))
      

(define pqueue-tag car)
(define pqueue-cmp< cadr)
(define pqueue-data caddr)

(define pqueue-empty? 
  (lambda (q)
    (null? (pqueue-data q))))

(define pqueue-head
  (lambda (q)
    (car (pqueue-data q))))

(define pqueue-rest
  (lambda (q)
    (make-pqueue (pqueue-cmp< q) (cdr (pqueue-data q)))))

(define pqueue-length 
  (lambda (q)
    (length (pqueue-data q))))

(define pqueue-insert
  (lambda (item pqueue)
    (let (cmp< (pqueue-cmp< pqueue))
      (define insert
        (lambda (item q)
          (cond ((null? q) (list item))
                ((cmp< item (car q))
                 (cons item q))
                (else
                 (cons (car q) (insert item (cdr q)))))))
      (make-pqueue cmp< (insert item (pqueue-data pqueue))))))


(define list->pqueue
  (lambda (lst cmp<)
    (cond ((null? lst) (make-pqueue cmp<))
          (else (pqueue-insert (car lst)
                               (list->pqueue (cdr lst) cmp<))))))
