#lang racket

;; Binary Trees

(define make-tree ; makes a binary tree
  (lambda (data left right)
    (list data left right)))

(define get-right
  (lambda (tree)
    (caddr tree)))

(define get-left
  (lambda (tree)
    (cadr tree)))

(define get-data
  (lambda (tree)
    (car tree)))

(define empty-tree null)
(define empty-tree? null?)

(define tree-from-leaf
  (lambda (data)
    (make-tree data empty-tree empty-tree)))

;;;;; ;;;;; ;;;;; ;;;;; ;;;;; ;;;;;

(define insert
  (lambda (item tree)
    (cond ((empty-tree? tree)
           (tree-from-leaf item))
          ((< item (get-data tree))
           (make-tree 
            (get-data tree)
            (insert item (get-left tree))
            (get-right tree)))
          ((> item (get-data tree))
           (make-tree 
            (get-data tree)
            (get-left tree)
            (insert item (get-right tree))))
          (else ; item equals the data in the tree
           tree))))
          

(define element?
  (lambda (item tree)
    (cond ((empty-tree? tree) #f)
          ((= item (get-data tree)) #t)
          ((< item (get-data tree))
           (element? item (get-left tree)))
          ((> item (get-data tree))
           (element? item (get-right tree))))))


(define list->tree
  (lambda (lst)
   (define list->tree-iter
     (lambda (lst tree)
       (cond ((null? lst) tree)
             (else (list->tree-iter (cdr lst)
                                    (insert (car lst) tree))))))
    (list->tree-iter lst empty-tree)))

;;;;; ;;;;; ;;;;; ;;;;; ;;;;; ;;;;;
; A few test trees

(define tree1 (list->tree (list 8 5 3 6 12 10 14)))
(define tree2 (list->tree (list 3 5 6 8 10 12 14)))

;;;;; ;;;;; ;;;;; ;;;;; ;;;;; ;;;;;

;; Useful binary tree functions:
;; 1. tree-map: apply a function to every element in the tree
;; 2. count-items: count the number of items in the tree
;; 3. count-leaves: count the number of leaves in the tree
;; 4. tree-height: determine the maximum height in the tree
;; 5. tree-accumulate: similar to list accumulate but using tree nodes

;; 1. tree-map: apply a function to every element in the tree
(define tree-map
  (lambda (fn tree)
    (cond ((empty-tree? tree) tree)
          (else (make-tree 
                 (fn (get-data tree)) 
                 (tree-map fn (get-left tree))
                 (tree-map fn (get-right tree)))))))


;; 2. count-items: count the number of items in the tree
(define count-items
  (lambda (tree)
    (cond ((empty-tree? tree) 0)
          (else (add1 (+ (count-items (get-left tree)) 
                         (count-items (get-right tree))))))))
  
;; 3. count-leaves: count the number of leaves in the tree
(define count-leaves
  (lambda (tree)
    (cond ((empty-tree? tree) 0)
          ((and (empty-tree? (get-left tree))
                (empty-tree? (get-right tree))) 1)
          (else (+ (count-leaves (get-left tree))
                   (count-leaves (get-right tree)))))))

;; 4. tree-height: determine the maximum height in the tree
;; An empty-tree returns 0; a tree with just a root returns 1.
(define tree-depth
  (lambda (tree)
    (cond ((empty-tree? tree) 0)
          (else (+ 1 (max (tree-depth (get-left tree))
                          (tree-depth (get-right tree))))))))

;; 5. tree-accumulate: similar to list accumulate but using tree nodes
(define tree-accumulate
  (lambda (tree combiner base term)
    (cond ((empty-tree? tree) base)
          (else (combiner 
		 (combiner 
		  (tree-accumulate (get-left tree) combiner base term) 
		  (term (get-data tree)))
		 (tree-accumulate (get-right tree) combiner base term))))))


;; Turn a tree into a sorted list
(define tree->list
  (lambda (tree)
    (tree-accumulate tree append null list)))


(provide empty-tree empty-tree? get-data get-left get-right 
         make-tree tree->list)

