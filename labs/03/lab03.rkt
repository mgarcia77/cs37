;; Madison Garcia and Melissa O'Connor, February 2013

#lang racket

(require rackunit)

;; Lab 03

;; The text (page 58;
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_idx_990)
;; provides a recursive process definition of sum:

(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a) 
           (sum term (next a) next b)))))

;; 1. Read what the sum function does.  Then, write an iterative
;; process version of sum called sum-i which has the same interface as
;; the function sum above. (This is Exercise 1.30)


(define sum-i
  (lambda (term a next b)
    (sum-helper term a next b 0)))

(define sum-helper
  (lambda (term a next b total)
    (if (> a b)
        total
        (sum-helper term (next a) next b (+ (term a) total)))))

(check-equal? (sum-i sub1 0 add1 4) (sum sub1 0 add1 4))

;; 2. Adapted from Exercise 1.31: Write a recursive (product-r) and
;; iterative (product-i) version of product.  Then, define factorial in
;; terms of product (either version since they should produce the same
;; answer given the same parameters).

(define product-r
  (lambda (term a next b)
    (if (> a b)
        1
        (* (term a) 
           (product-r term (next a) next b)))))


(define product-i
  (lambda (term a next b)
    (product-helper term a next b 1)))

(define product-helper
  (lambda (term a next b total)
    (if (> a b)
        total
        (product-helper term (next a) next b (* (term a) total)))))


(define factorial
  (lambda (x)
    (product-i (lambda (y) y) 1 add1 x)))  ; term is identity


(check-equal? (product-r add1 0 add1 4) 120)
(check-equal? (product-r add1 0 add1 4) (product-i add1 0 add1 4))

(check-equal? (factorial 1) 1)
(check-equal? (factorial 5) 120)



#|
3. The following is taken from the text, Exercise 1.32:

"Accumulate takes as arguments the same term and range specifications
as sum and product, together with a combiner procedure (of two
arguments) that specifies how the current term is to be combined with
the accumulation of the preceding terms and a null-value that
specifies what base value to use when the terms run out. Write
accumulate and show how sum and product can both be defined as simple
calls to accumulate."

Your job is to write a recursive process and an interative process
solution for accumulate, called accumulate and accumulate-i,
respectively.  Both functions should have the same interface:
|#



(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b)))))
        

    
(define accumulate-i
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (accumulate-i combiner (combiner null-value (term a)) term (next a) next b))))

; sum and product can be called using accumulate by having + and 0 as the combiner and null-value (sum)
; or * and 1 as the combiner and null-value (product) as below:

(check-equal? (accumulate + 0 add1 0 add1 4) (sum add1 0 add1 4) (accumulate-i + 0 add1 0 add1 4))

(check-equal? (accumulate * 1 add1 0 add1 4) (product-r add1 1 add1 4) (accumulate-i * 1 add1 0 add1 4))




;; 4. Exercise 1.33 describes a function called filtered-accumulate.
;; The description below is taken from exercise 1.33 but the questions
;; are different.

#|
You can obtain an even more general version of accumulate (exercise
1.32) by introducing the notion of a filter on the terms to be
combined. That is, combine only those terms derived from values in the
range that satisfy a specified condition. The resulting
filtered-accumulate abstraction takes the same arguments as
accumulate, together with an additional predicate of one argument that
specifies the filter. 

The function filtered-accumulate should have the following form:
----> (accumulate combiner null-value term a next b filter)

Write filtered-accumulate as a recursive process procedure. Show how
to express the following using filtered-accumulate:

a. the sum of the square-roots of the numbers that are divisible by 7
in the interval a to b.  You may want to write a function called
divisible-by-seven that returns true if it's argument is divisible by
seven.  You will probably want to use the built-in functions modulo
and sqrt in your solution.  (NOTE: Don't just use the next function to
count by 7's. Instead, count by 1's but use the filter to only include
values that are divisible by 7.)

b. the product of all the positive integers less than n that are
perfect squares (i.e., all positive integers i < n such that
there exists some other positive integer j where j*j = i).  You can 
accomplish this test using the built-in functions exact? and sqrt.

|#

(define filtered-accumulate
  (lambda (combiner null-value term a next b filter)
    (if (> a b)
        null-value
        (if (filter a)
            (combiner (term a)
                      (filtered-accumulate combiner null-value term (next a) next b filter))
            (filtered-accumulate combiner null-value term (next a) next b filter)))))


(define c 40)
(define d 50)

;; part a
(define divisible-by-seven
  (lambda (x)
    (if (= (modulo x 7) 0) #t #f)))
    
(check-equal? (filtered-accumulate + 0 sqrt c add1 d divisible-by-seven) 13.48074069840786)


;; part b
(define perfect-square
  (lambda (x)
    (if (exact? (sqrt x)) #t #f)))

(check-equal? (filtered-accumulate * 1 identity c add1 d perfect-square) 49)
(check-equal? (filtered-accumulate * 1 identity 1 add1 17 perfect-square) (* 1 4 9 16))


;; 5.  Write a function called "make-exponentiator" which takes a
;; single parameter, e (an exponent), and returns a function of one
;; parameter, n, which it raises to the e-th power.  Use the Racket
;; function expt to help in your solution.  (expt n e) raises n to the
;; e-th power.  You only need to handle integer exponenets greater
;; than or equal to 0.


(define make-exponentiator
  (lambda (e)
    (lambda (n) (expt n e))))

;; For example:
(define square (make-exponentiator 2))
(check-equal? (square 4) 16) ;; => 16

(define cube-root (make-exponentiator 1/3))
(check-equal? (cube-root 27) 3.0) ;; => 3.0




;; 6. Complete exercise 1.41:

#|
Define a procedure double that takes a procedure of one argument as an
argument and returns a procedure that applies the original procedure
twice. For example, if add1 is a procedure that adds 1 to its
argument, then (double add1) should be a procedure that adds 2.

Before you have completed that, try to figure out what value is returned by:

(((double (double double)) add1) 5)

Explain why the result you got is the correct answer.
|#

(define double
  (lambda (proc)
    (lambda (x) (proc (proc x)))))

(check-equal? (((double (double double)) add1) 5) 21)  ;; => 21


;; When we expand what happens with all the doubles, we get
;; ((double (double double)) add1)
;; (double( double(double(add1) ))
;; (double(double (double(double(add1)))))
;; (double(double (double(add2))))
;; (doulbe(double (add4)))
;; (double(add8))
;; (add16)
;; Therefore, 16+5=21.


;; 7. Complete exercise 1.42:

#|
Let f and g be two one-argument functions. The composition f after g
is defined to be the function x --> f(g(x)). Define a procedure
compose that implements composition. For example, if inc is a
procedure that adds 1 to its argument,

((compose square inc) 6)   ; --> (square (inc 6)) --> 49 
|#

(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(check-equal? ((compose square add1) 6) 49)
(check-equal? ((compose sub1 square) 5) 24)



;; 8. Complete exercise 1.43:


#|
If f is a numerical function and n is a positive integer, then we can
form the nth repeated application of f, which is defined to be the
function whose value at x is f(f(...(f(x))...)). For example, if f is
the function x -> x + 1, then the nth repeated application of f is the
function x -> x + n. If f is the operation of squaring a number, then the
nth repeated application of f is the function that raises its argument
to the (2^n)th power. Write a procedure that takes as input a procedure
that computes f and a positive integer n and returns the procedure
that computes the nth repeated application of f. Your procedure should
be able to be used as follows:

((repeated square 2) 5)  ; (square (square 5)) -> 625
((repeated add1 5) 0)  ; (add1 (add1 (add1 (add1 (add1 0))))) -> 5

Hint: You may find it convenient to use compose from exercise 1.42
|#

(define repeated
  (lambda (f n)
    (cond ((<= n 0) identity)
          (else (compose f (repeated f (sub1 n)))))))
    
(check-equal? ((repeated square 2) 5) 625)
(check-equal? ((repeated add1 5) 0) 5)
(check-equal? ((repeated sub1 0) 5) 5)


;; We will talk about "deep lists" and "deep recursion" on Tuesday.


;9. Write a function called add1-to-each* which adds 1 to each element
;of a deep list.

(define add1-to-each*
  (lambda (lst)
    (cond ((null? lst) null)
          ((pair? (car lst)) (cons (add1-to-each* (car lst))
                                   (add1-to-each* (cdr lst))))
          (else (cons (+ 1 (car lst)) 
                      (add1-to-each* (cdr lst)))))))

(check-equal? (add1-to-each* '(1 (2 3) (4 (5 6)))) '(2 (3 4) (5 (6 7))))

;10. Write a function called operate-on-each* which returns a function
;that performs an operation on each element of a deep list.


(define operate-on-each*
  (lambda (op)
    (define func*
      (lambda (lst)
        (cond ((null? lst) null)
              ((pair? (car lst)) (cons (func* (car lst))
                                       (func* (cdr lst))))
              (else (cons (op (car lst))
                          (func* (cdr lst)))))))
    func*))


(define double-each* (operate-on-each* (lambda (n) (* n 2))))
(check-equal? (double-each* '(1 (2 (3)))) '(2 (4 (6)))) ; --> (2 (4 (6)))