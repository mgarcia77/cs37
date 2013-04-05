; Madison Garcia and Melissa O'Connor

#lang racket

(require rackunit)

;; Lab 02
;; Using lists with recursion and iteration

;; There are a total of 13 questions below. The first 9 questions
;; require a recursive process solution. The next 3 questions require
;; an iterative process solution. You can solve the final question 
;; either way. Two of the questions are "Challenge" questions and are
;; optional.

;; You can assume that each of the functions will be called with
;; arguments of the proper data type (e.g. a list, an integer, etc).  

;; Although each question asks you to write one function, you are always
;; welcome to write additional "helper" functions as needed, provided
;; the function I am asking you write works as described.


;; ------------------------------------------------------------

;; 0. Warmup. In this question, I will guide you through the answer
;; to a question that involves building up a list recursively. We will
;; write a function that takes a list of numbers, lst, as one
;; parameter and a number, n, as a second parameter. The result of
;; the function is a new list where every element in the list has been
;; increased by n. 
;;
;; There are two keys concepts for this question.

;; a. The base case: When there are no elements in lst, the result of
;; the function is the empty list.

;; b. The recursive step: When there are elements left in lst, the
;; result of the function is what you'd get by creating a new list
;; whose first element is equal to what you'd get by adding the first
;; of element of lst to n and whose remaining elements are what you'd
;; get by calling the function recursively on the rest of the
;; elements.

;; Remember that all functions should use the lambda notation,
;; not the shortcut notation found in the text. For example,
;; your solution to this question should begin something like
;; the following:

;; (define add-to-lst
;;   (lambda (lst n)
;;     ...
;;      ))

;; Here is one solution to the question:

(define add-to-lst
  (lambda (lst n)
    (cond ((null? lst) null)   ; the base case
	  (else                ; the recursive step
	   (cons               ; create a new list
	    (+ (car lst) n)    ; whose first elt is (car lst) + n
	    (add-to-lst        ; and remaining elts by calling fn recursively
	     (cdr lst) n)))))) ; on the rest of the elements 


;; Normally we wouldn't write the function spread across so many
;; lines. I did so above to make the comments easier to read. Here's
;; a more stylistically appropriate way to write it. (Note that I need
;; to change the name of the function because I can't re-define
;; add-to-lst.)

(define list-adder
  (lambda (lst n)
    (cond ((null? lst) null)
	  (else (cons (+ (car lst) n) (list-adder (cdr lst) n))))))


;; Of course, we should write test cases for our function!

(check-equal? (list-adder '(1 2 3) 1) '(2 3 4))
(check-equal? (list-adder '(2 4 6) -4) '(-2 0 2))
(check-equal? (list-adder '() 2) null)



;; ------------------------------------------------------------

;; 1. Write a function called xerox that takes an item, elt, and a
;; number, n, and returns a list containing n copies of elt. If n is
;; less than or equal to 0, xerox returns the empty list.

;; Remember, your solution should begin like this:
;;
;; (define xerox
;;   (lambda (elt n)
;;      ...
;;      ))

(define xerox
  (lambda (elt n)
    (cond ((<= n 0) '())
          (else (cons elt (xerox elt (- n 1)))))))

;; Below are some test cases for the "xerox" function you will
;; write. You should add some more test cases. They need not be
;; exhaustive, but they should check more than just a few basic test
;; cases. In subsequent questions, you will write most or all of the
;; test cases yourself.

;; Uncomment the tests once you have written the function.

;; Test cases:
 (check-equal? (xerox 'paper 2) '(paper paper))
 (check-equal? (xerox 'paper 0) null)



;; ------------------------------------------------------------

;; 2. Write a function called "filter-out" that takes two parameters.
;; The first parameter is a list and the second parameter is something
;; you'd like to remove from that list. The result is that all
;; appearances of the item you'd like to filter out are removed. You
;; should use "equal?" to test for equality.

(define filter-out
  (lambda (lst elt)
    (cond ((null? lst) '())
          ((equal? (car lst) elt) (filter-out (cdr lst) elt))
          (else (cons (car lst) (filter-out (cdr lst) elt))))))



;; Test cases:
 (check-equal? (filter-out '(1 2 1 3 1 4 1) 1) '(2 3 4))
 (check-equal? (filter-out '(1 2 1 3 1 4 1) 5) '(1 2 1 3 1 4 1))
 (check-equal? (filter-out '() 5) null)



;; ------------------------------------------------------------

;; 3. Write a function called "element?" which tests to see if an
;; element is a member of a list. You may not use the built-in
;; function "member" in your solution.

(define element?
  (lambda (elt lst)
    (cond ((null? lst) #f)
          ((equal? (car lst) elt) #t)
          (else (element? elt (cdr lst))))))



;; Test cases:
 (check-equal? (element? 2 '(1 2 3)) #t)
 (check-equal? (element? 4 '()) #f)
 (check-equal? (element? 5 '(1 2 3)) #f)



;; ------------------------------------------------------------

;; 4. Write a function called "all-in-range?" that has three parameters.
;; The first parameter is a number representing the low value of the
;; range. The second parameter is a number representing the high
;; value of the range. The third parameter is a list of numbers. The
;; function returns true only if every element of the list is greater 
;; than or equal to the low value of the range and less than or equal
;; to the high value of the range. Return #t if the list is empty.

(define all-in-range?
  (lambda (low high lst)
    (cond ((null? lst) #t)
          ((or (< (car lst) low) (> (car lst) high)) #f)
          (else (all-in-range? low high (cdr lst))))))



;; Test cases:
 (check-equal? (all-in-range? 1 10 '(1 3 5)) #t)
 (check-equal? (all-in-range? 1 10 '(5 10 15)) #f)
 (check-equal? (all-in-range? 1 10 '()) #t)



;; ------------------------------------------------------------

;; 5. Write a function called swap that takes an item x, an item y,
;; and a list lst and returns a new list with all occurrences of x
;; replaced by y, and all occurrences of y replaced by x.

(define swap
  (lambda (x y lst)
    (cond ((null? lst) '())
          ((equal? (car lst) x) (cons y (swap x y (cdr lst))))
          ((equal? (car lst) y) (cons x (swap x y (cdr lst))))
          (else (cons (car lst) (swap x y (cdr lst)))))))




;; Test case:
 (check-equal? (swap 'red 'blue '(red fish red fish blue)) 
               '(blue fish blue fish red))


;; ------------------------------------------------------------

;; 6. Write a function called "add-pairs" that takes a list of numbers
;; and adds up consecutive items in the list. If there is only one
;; element left in the list, just add 0 to it. If there are no
;; elements in the list, this evaluates to the empty list. 

(define add-pairs
  (lambda (lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) (cons (+ (car lst) 0) null))
          (else (cons (+ (car lst) (car (cdr lst))) (add-pairs (cddr lst)))))))



;; Test cases:
 (check-equal? (add-pairs '(8 6 7 5 3 0 9)) '(14 12 3 9))
 (check-equal? (add-pairs '()) null)


;; ------------------------------------------------------------

;; 7. *Challenge* Can you write a function called "sum" that uses that
;; add-pairs function repeatedly to add all the numbers in the list?
;; For example:
;; (add-pairs '(8 6 7 5 3 0 9)) ; => '(14 12 3 9)
;; (add-pairs '(14 12 3 9)) ; => '(26 12)
;; (add-pairs '(26 12)) ; => '(38) <--- that's our sum

(define sum
  (lambda (lst)
    (cond ((null? lst) 0)
          ((null? (cdr lst)) (car lst))
          (else (sum (add-pairs lst))))))
 
;; Write your own test cases:
(check-equal? (sum '(8 6 7 5 3 0 9)) 38)
(check-equal? (sum '()) 0)
(check-equal? (sum '(1 2 3 4 5)) 15)


;; ------------------------------------------------------------

;; 8. Write a function called "merge" that takes two lists as
;; parameters and merges them together by first taking one element
;; from the first list, then one from the second list, etc. Once one
;; of the lists is exhausted, just add all the remaining elements from
;; the non-empty list to the end.

(define merge
  (lambda (lst1 lst2)
    (cond ((null? lst1) lst2)
          ((null? lst2) lst1)
          (else (cons (car lst1) 
                      (cons (car lst2) 
                            (merge (cdr lst1) (cdr lst2))))))))



;; Test cases:
 (check-equal? (merge '(a b a c a b) '(y y z)) '(a y b y a z c a b))


;; ------------------------------------------------------------

;; 9. Note: This problem is more difficult than it may initially seem.
;;
;; Write a function called list-replace that takes a list of two
;; element lists, replacements, and a list, lst, and returns a new
;; list with the first element of each pair replaced by the second
;; element of each pair. Be sure your function can handle both test
;; cases below. (The second test case is the harder one. You may need
;; to write a second 'helper' function to aid in the solution of this
;; question.)

(define list-replace
  (lambda (replacements lst)
    (cond ((null? lst) '())
          (else (cons (find-replacement (car lst) replacements)
                      (list-replace replacements (cdr lst))))
          )))
           
; helper function for list-replace
; finds the value that will replace the current value
; if current value isn't in the replacements list, value replaces itself
(define find-replacement
  (lambda (current replacements)
    (cond ((null? replacements) current)
          ((equal? current (car (car replacements))) (cadr (car replacements)))
          (else (find-replacement current (cdr replacements))))))



          

;; Test cases:
 (check-equal? (list-replace '((carrots peas) (fork spoon)) 
                             '(eat your carrots with a fork))
               '(eat your peas with a spoon))

 (check-equal? (list-replace '((carrots peas) (peas potatoes))
                             '(eat your carrots and peas))              
               '(eat your peas and potatoes))
;; THE ANSWER IS NOT: (eat your potatoes and potatoes)


;; ------------------------------------------------------------
;; ITERATIVE PROCESS EXERCISES
;; ------------------------------------------------------------

;; 10. Rewrite the xerox function from Question 1 as an iterative
;; process solution (rather than a recursive process solution).
;; This new version should be called xerox-i. Be sure to write tests.


(define xerox-helper
  (lambda (lst elt n)
    (cond ((<= n 0) lst)
          (else (xerox-helper (cons elt lst) elt (sub1 n))))))

(define xerox-i
  (lambda (elt n)
    (xerox-helper '() elt n)))

;; Test cases:
 (check-equal? (xerox-i 'paper 2) '(paper paper))
 (check-equal? (xerox-i 'paper 0) null)
 
 (check-equal? (xerox 'paper 2) (xerox-i 'paper 2))
 (check-equal? (xerox 'paper 0) (xerox-i 'paper 0))

;; ------------------------------------------------------------

;; 11. Rewrite the swap function from Question 5 as an iterative
;; process solution. This new version should be called swap-i.
;; Be sure to write tests. Your solution may contain 'reverse' but
;; may not contain 'append'.


(define swap-i
  (lambda (x y lst)
    (swap-helper x y lst '())))

(define swap-helper
  (lambda (x y lst result)
    (cond ((null? lst) (reverse result))
          ((equal? (car lst) x) (swap-helper x y (cdr lst) (cons y result)))
          ((equal? (car lst) y) (swap-helper x y (cdr lst) (cons x result)))
          (else (swap-helper x y (cdr lst) (cons (car lst) result))))))

;; Your tests:

(check-equal? (swap-i 'red 'blue '(red blue red)) '(blue red blue))
(check-equal? (swap 'red 'blue '(red fish red fish blue)) 
               (swap-i 'red 'blue '(red fish red fish blue)))


;; ------------------------------------------------------------

;; 12. Rewrite the add-pairs function from Question 6 as an iterative
;; process solution. This new version should be called add-pairs-i.
;; Be sure to write tests. Your solution may contain 'reverse' but
;; may not contain 'append'.

(define add-pairs-i
  (lambda (lst)
    (reverse (add-helper lst '()))))

(define add-helper
  (lambda (lst result)
    (cond ((null? lst) result)
          ((null? (cdr lst)) (cons (car lst) result))
          (else (add-helper (cddr lst) (cons (+ (car lst) (cadr lst)) result))))))


;; Test cases:
 (check-equal? (add-pairs '(8 6 7 5 3 0 9)) (add-pairs-i '(8 6 7 5 3 0 9)))
 (check-equal? (add-pairs '()) (add-pairs-i '()))
 (check-equal? (add-pairs '(2)) (add-pairs-i '(2)))

;; ------------------------------------------------------------

;; 13. *Challenge* Write a function called "unmerge" that takes one
;; list as a parameter and splits them into a list of two lists. The
;; end result can be described by saying the elements in odd numbered
;; positions from the original list end up in the first list, and
;; elements in even numbered positions end up in the second list,
;; though that is not necessarily the algorithm you would use.

;; You can use either an iterative process or recursive process solution.

;; ITERATIVE UNMERGER
(define unmerge-i
  (lambda (lst)
    (unmerge-helper lst '() '())))

(define unmerge-helper
  (lambda (lst result1 result2)
    (cond ((null? lst) (list (reverse result1) 
                             (reverse result2)))
          ((null? (cdr lst)) (list (reverse (cons (car lst) result1))
                                   (reverse result2)))
          (else (unmerge-helper (cddr lst) 
                                (cons (car lst) result1) 
                                (cons (cadr lst) result2))))))


;; RECURSIVE UNMERGER
(define unmerge-r
  (lambda (lst)
    (cond ((null? lst) '(() ()))
          ((null? (cdr lst)) (list lst '()))
          (else (list (odd-elts lst) (odd-elts (cdr lst)))))))

; helper function for unmerge-r
; given a list, returns a list containing every other elt from the list
(define odd-elts
  (lambda (lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) lst)
          (else (cons (car lst) (odd-elts (cddr lst)))))))


;; Test cases:
 (check-equal? (unmerge-i '(a y b y a z c a b)) '((a b a c b) (y y z a)))
 (check-equal? (unmerge-i '()) '(() ()))
 (check-equal? (unmerge-i '(a x b y c z)) '((a b c) (x y z)))

 (check-equal? (unmerge-r '(a y b y a z c a b)) '((a b a c b) (y y z a)))
 (check-equal? (unmerge-r '()) '(() ()))
 (check-equal? (unmerge-r '(a x b y c z)) '((a b c) (x y z)))
 
;; ------------------------------------------------------------
;; EXTRA PROBLEM
;; given a list of non-negative ints, output list with each elt
;; in it that many times
;; example: (super-xerox '(1 2 3)) --> '(1 2 2 3 3 3)
 
; helper function performs copying step for a given elt 
(define super-xerox-helper
  (lambda (elt n lst)
    (cond ((<= n 0) lst)
          (else (cons elt (super-xerox-helper elt (sub1 n) lst))))))

; recursive function that calls helper to build list
(define super-xerox
  (lambda (lst)
    (cond ((null? lst) '())
          (else (super-xerox-helper (car lst) (car lst)
                                    (super-xerox (cdr lst)))))))

(check-equal? (super-xerox '(1 2 3)) '(1 2 2 3 3 3))
(check-equal? (super-xerox '(0 4)) '(4 4 4 4))
(check-equal? (super-xerox '()) '())
(check-equal? (super-xerox '(1 2 1)) '(1 2 2 1))
(check-equal? (super-xerox '(-5)) '())