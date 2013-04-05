#lang racket

(require "btree.rkt")

; ****************************************************************************
; SET OPERATIONS
; (element? item set)      determines if the item is a member of the set
; (adjoin item set)        adds the item to the set to create a new set
; (intersection set1 set2) returns a set which contains only the items 
;			   that are in both set1 and set2
; (union set1 set2)        returns a set which contains all the items
;                          that are in either set1 or set2
; ****************************************************************************

; OPTION 4: Binary trees

(define empty-set empty-tree)

; Let's define the four set functions based on the
; binary tree abstraction:

(define element?
  (lambda (item set)
    (cond
     ((empty-tree? set) #f)
     ((= item (get-data set)) #t)
     ((< item (get-data set)) (element? item (get-left set)))
     (else (element? item (get-right set))))))


(define adjoin
  (lambda (item set)
    (cond
     ((empty-tree? set) (make-tree item '() '()))
     ((= item (get-data set)) set)
     ((< item (get-data set))
      (make-tree 
       (get-data set)
       (adjoin item (get-left set))
       (get-right set)))
     (else
      (make-tree
       (get-data set)
       (get-left set)
       (adjoin item (get-right set)))))))

; Complete union as an O(nlgn) function, assuming
; the binary tree remains relatively balanced.
(define union 
  (lambda (s1 s2)
    #t))

; Complete intersection as an O(nlgn) function, assuming
; the binary tree remains relatively balanced.
(define intersection
  (lambda (s1 s2)
    #t))

; sets to test with
(define set1 (adjoin 1 (adjoin 12 (adjoin 15 (adjoin 5 (adjoin 10 empty-set))))))
(define set2 (adjoin 1 (adjoin 9 (adjoin 7 (adjoin 5 empty-set)))))

