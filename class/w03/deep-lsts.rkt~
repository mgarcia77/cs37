#lang racket

;; working with nested lists

(define member?
  (lambda (elt lst)
    (cond ((null? lst) #f)
          ((equal? elt (car lst)) #t)
          (else (member? elt (cdr lst))))))

(member? 3 '(4 5 6 2 3 9))  ;; #t
(member? 3 '(4 5 6 2 7 9))  ;; #f

(define lst '(4 (5 6) (2 (3 9))))

(member? 3 lst)  ;; compares 3 to 4, '(5 6), '(2 (3 9)) => #f

#|
;; list? => #t if '() or other lists (including nested)
;; pair? => #t if pointing to cons pair (even if not valid list)
(list? '())  ;; #t
(list? '(1 2 3))  ;; #t
(pair? '(1 2 3))  ;; #t
(pair? null)  ;; #f
(list? (cons 3 4))  ;; #f (not a list)
(pair? (cons 3 4))  ;; #t (is a pair)
|#

;; * indicates deep (nested) list
(define member*?
  (lambda (elt lst)
    (cond ((null? lst) #f)
          ((pair? (car lst)) (or (member*? elt (car lst))    ;; when we find a pair, must look in
                                 (member*? elt (cdr lst))))  ;; list in car and list in cdr
          ((equal? elt (car lst)) #t)
          (else (member*? elt (cdr lst))))))

(member*? 3 lst)  ;; #t


(define sum
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (+ (car lst) (sum (cdr lst)))))))

(sum '(1 2 3 4 5))  ;; 15
;(sum '(1 2 ((3 4) 5)))  ;; should be 15
     

(define sum*
  (lambda (lst)
    (cond ((null? lst) 0)
          ;((number? lst) lst)  ;; if we have pairs that aren't lists
          ((not (pair? lst)) lst)
          ((pair? (car lst)) (+ (sum* (car lst)) 
                                (sum* (cdr lst))))
          (else (+ (car lst) (sum* (cdr lst)))))))

(sum* '(1 2 (3 (4 5))))  ;; 15
(sum* '(1 . 2))  ;; 3
(sum* '(((1 2) (3 (4 (5))) 6 (7 (8 . 7)) (9 10))))  ;; 62
    