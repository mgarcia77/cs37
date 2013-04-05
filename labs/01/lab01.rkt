#lang racket

; YOUR NAME (AND YOUR PARTNER'S NAME IF YOU HAD ONE)
; Lab 01

; Due Sunday, January 27 at 11:59pm

; Please read the README file included in this directory.  You can
; read the file in DrRacket's editor simply by opening it using
; the File menu.

; 1. Complete exercise 1.2 (page 21), rewritten here:

;    Translate the following expression into prefix form
;    (5 + 4 + (2 - (3 - (6 + 4/5)))) / (3 * (6 - 2) * (2 - 7))

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; 2. Write a procedure which takes three numbers as arguments
;    and returns the median (middle) of the three values. Use
;    *if* in your solution instead of *cond*.

(define median
  (lambda (x y z)
    (if (and (< x y) (< x z)) (if (< y z) y z) 
        ; if x is smallest, the smaller of y or z is median
        (if (and (> x y) (> x z)) (if (< y z) z y) 
        ; if x is largest, the larger of y or z is median
        x)))) ; otherwise x is median

; For example:  (Uncomment and use as test cases when you're done)
(median 3 4 5); ==> 4
(median 6 1 8); ==> 6
(median 5 5 6); ==> 5


; 3. Write a procedure called fica-tax which computes the 
;    amount of social security tax plus medicare tax. Use *cond*
;    in your solution instead of *if*.

;    For 2013, the social security tax is 6.2% of income up to
;    $113,700. No social security tax is paid on earnings in excess of
;    $113,700.  For 2013, the medicare tax is 1.45% of earnings up to
;    $200,000.  For *excess* income above $200,000, the medicare tax
;    rises to 2.35%.

(define fica
  (lambda (income)
    (cond ((<= income 113700) (+ (* income 0.062) (* income 0.0145)))
             ; lower bracket: 6.2% of income + 1.45% of income
          ((< income 200000)
               (+ (* 113700 0.62) (* income 0.0145)))
             ; middle bracket: 6.2% of $113,700 + 1.45% of income
          (else 
               (+ (* 0.062 113700) (* 200000 0.0145) 
                  (* (- income 200000) 0.0235))))))
             ; upper bracket: 6.2% of $113,700 + 1.45% of $200,000 + 2.35% rem

; For example:  (Uncomment and use as test cases when you're done)
 (fica 25000) ; ==> 1912.5
 (fica 113000); ==> 8644.5
 (fica 200000); ==> 9949.4
 (fica 201000); ==> 9972.9



; 4. Complete exercise 1.4 (page 21), rewritten here:

; Observe that our model of evaluation allows for combinations whose
; operators are compound expressions. Use this observation to describe
; the behavior of the following procedure:

(define a-plus-abs-b
  (lambda (a b)
    ((if (> b 0) + -) a b)))


; want to add a + |b|
; if lets us determine whether we need to add or subtract b to add abs.
; since functions are things, if lets us return either + or - as the operation
; to perform on a and b. order of evaluation first determines the operator,
; then applies it.




; 5. Complete exercise 1.6 (page 25) -- not transcribed here.  Find the 
; question in the text.  You will have to read Section 1.1.7 first, too.
; Here is a link to the question:
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_thm_1.6

; Below is the supporting code taken from SICP pages 23-25.

; Note that I have converted the book's "shortcut" function notation to
; the lambda notation we will use in class.

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x))))

; Taken from SICP page 23
(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

; Taken from SICP page 23
(define average
  (lambda (x y)
    (/ (+ x y) 2)))

; Taken from SICP page 24
(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

; Not in the book, but you need it for the good-enough? function above.
(define square
  (lambda (x)
    (* x x)))

; Taken from SICP page 24 (though I changed the name from sqrt)
(define square-root 
  (lambda (x)
    (sqrt-iter 1.0 x)))

; Taken from SICP page 25
(define new-if
  (lambda (predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause))))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x)
                     x)))
; (sqrt-iter2 1 9)

; After modifying the sqrt-iter procedure as described in 1.6, answer:
; a. What happens when Alyssa attempts to use this to compute square 
;    roots?  Explain.
; b. Come up with another example where "new-if" does not work as you would
;    expect "if" to work.

; (define x 0)
; (new-if (= x 0) x (/ 100 x))

; 6. Complete exercise 1.8 (page 26)
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_thm_1.8

; (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)