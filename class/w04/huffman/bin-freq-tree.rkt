#lang racket

#|
Binary Frequency Trees where each node is a list-of-symbols and a freq.

NOTE: These are not binary search trees. No insert method is provided.
|#


(provide make-tree empty-tree empty-tree? 
         get-right get-left get-root 
         merge-trees make-node node-symbols node-freq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nodes

(define make-node
  (lambda (symbols freq)
    (cons symbols freq)))

(define node-symbols car)

(define node-freq cdr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trees

(define make-tree
  (lambda (root left right)
    (list root left right)))

(define get-root
  (lambda (tree)
    (car tree)))

(define get-left
  (lambda (tree)
    (cadr tree)))

(define get-right
  (lambda (tree)
    (caddr tree))) 

(define empty-tree '())
(define empty-tree? null?)

(define merge-trees
  (lambda (tree1 tree2)
    (cond ((empty? tree1) tree2)
          ((empty? tree2) tree1)
          (else 
           (let ((symbols (append (node-symbols (get-root tree1))
                                  (node-symbols (get-root tree2))))
                 (freq (+ (node-freq (get-root tree1))
                          (node-freq (get-root tree2)))))
             (make-tree
              (make-node symbols freq)
              tree1
              tree2))))))

