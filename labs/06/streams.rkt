#lang racket

(require rackunit)

(define-syntax-rule (delay-expression e)
  (lambda () e))

(define force-expression
  (lambda (e)
    (e)))

(define-syntax-rule (scons x y)
  (mcons x (delay-expression y)))

(define scar
  (lambda (s)
    (mcar s)))

(define scdr
  (lambda (s)
    (force-expression (mcdr s))))

(define snull null)

(define snull?
  (lambda (s)
    (eq? s snull)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here is the very useful stream-combiner procedure:

(define stream-combiner
  (lambda (op)
    (lambda (s1 s2)
      (cond ((or (snull? s1) (snull? s2)) snull)
            (else 
             (scons (op (scar s1) (scar s2))
                    ((stream-combiner op)
                     (scdr s1) (scdr s2))))))))

(define add-streams (stream-combiner +))
(define sub-streams (stream-combiner -))
(define mul-streams (stream-combiner *))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are some more procedures we wrote in class:

;; returns a *list* of the first n items of a stream
(define sfirst
  (lambda (n s)
    (cond ((or (<= n 0) (snull? s)) null)
          (else
           (cons (scar s) (sfirst (sub1 n) (scdr s)))))))

;; create a stream of integers between low and high, inclusive
(define senumerate
  (lambda (low high)
    (cond ((> low high) snull)
          (else
           (scons low (senumerate (add1 low) high))))))

;; creates a new stream whose values are the result of applying the
;; procedure proc to each elements of stream
(define smap
  (lambda (proc s)
    (cond ((snull? s) snull)
          (else
           (scons (proc (scar s)) (smap proc (scdr s)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are some new stream procedures:

;; a procedure to make finite streams out of finite lists
(define list->stream
  (lambda (lst)
    (cond ((null? lst) snull)
          (else
           (scons (car lst) 
                  (list->stream (cdr lst)))))))

;; append two streams: does not modify the original streams 
(define sappend
  (lambda (s1 s2)
    (cond ((snull? s1) s2)
          ((snull? s2) s1)
          (else (scons (scar s1)
                       (sappend (scdr s1) s2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are some infinite streams:

(define whole (scons 0 (smap add1 whole)))

;; These are two "obvious" streams which we can derive from whole:

(define positives (scdr whole))
(define negatives (smap - positives))

;; EXERCISE 0
;;
;; Can you figure out the values stored in the following infinite
;; stream without running it?
;;

(define mystery (scons 1 (add-streams mystery mystery)))

;; Mystery stores the powers of 2. It starts at 1 and each successive
 ; item is the result of adding the previous item with itself.

;; EXERCISE 1 
;;
;; How can we define the integers (-inf, inf)?  We could try
;; sappend'ing all the negatives onto all the positives as follows:
;;
(define integers-first-attempt (sappend negatives positives))
;;
;; a. Why doesn't this work?
;;

;; This returns all the negatives in ascending order (by magnitude)
 ; followed by all the positives in ascending order. This is 
 ; effectively the same as the negatives stream, since there are
 ; infinitely many negative integers so we'll never find the positives.

;; b. To fix this, we need a strategy of which takes from the first
;; stream, then the second stream, then the first stream, then the
;; second, etc.  This is usually called "zip" because it zips up two
;; lists the way a zipper works. (In Python and Haskell, there is a
;; built-in procedure called "zip", though its behavior is slightly
;; different than described here.)  If you exhaust one stream, the
;; procedure ends.  For example, zip on two lists (not streams) might
;; look like this: (zip '(1 2 3) '(4 5)) -> (1 4 2 5))
;;
;; Write the stream version "szip" then demonstrate that your answer
;; is correct. 

(define szip
  (lambda (s1 s2)
    (cond ((or (snull? s1) (snull? s2)) snull)
          (else (scons (scar s1) 
                       (scons (scar s2)
                              (szip (scdr s1) (scdr s2))))))))
  
(define integers (szip whole negatives))


;; Using smap and szip, we can create a stream which includes all the
;; multiples of 2 and the multiples of 3:

(define multiples-of-2 
  (smap (lambda (x) (* 2 x)) positives))

(define multiples-of-3
  (smap (lambda (x) (* 3 x)) positives))

(define multiples-of-2-and-3
  (szip multiples-of-2 multiples-of-3))

;; c. Are there any problems with the definition of
;; multiples-of-2-and-3 above?

;; multiples-of-2-and-3 will include all multiples of 6 twice: once for
;; being in multiples-of-2 and once for being in multiples-of-3. It also
;; doesn't have its items in order. We get 1*2, 1*3, 2*2, 2*3, 3*2, 3*3,
;; 4*2, 4*3... even though this comes out to the unordered stream 2, 3,
;; 4, 6, 6, 9, 8, 12...


;; EXERCISE 2
;;
;; What if, instead of szipping two streams, we wanted two streams to
;; be sorted as they were merged?  What if we wanted this sorted
;; stream to contain no duplicates? (Hint: The solution is in the text
;; if you get stuck, but try and solve it on your own).  This solution
;; assumes both streams are already sorted from smallest to largest.
;; This is true for the multiples-of-2 and multiples-of-3 streams
;; listed above.

(define smerge
  (lambda (s1 s2)
    (cond ((snull? s1) s2)
          ((snull? s2) s1)
          ((< (scar s1) (scar s2)) (scons (scar s1) 
                                          (smerge (scdr s1) s2)))
          ((= (scar s1) (scar s2)) (smerge (scdr s1) s2))
          (else (scons (scar s2)
                       (smerge s1 (scdr s2)))))))

(define smerged-multiples-of-2-and-3
  (smerge multiples-of-2 multiples-of-3))

;; Demonstrate that your answer is correct.

(check-equal? (sfirst 15 smerged-multiples-of-2-and-3) '(2 3 4 6 8 9 10 12 14 15 16 18 20 21 22))
(check-equal? (sfirst 5 (smerge (scons 1 (scons 2 snull)) (scons 3 (scons 4 (scons 5 snull))))) '(1 2 3 4 5))

;; EXERCISE 3
;;
;; What if we wanted to compute the intersection of two streams
;; (assuming both streams are already sorted and have no duplicates)?

(define sintersect
  (lambda (s1 s2)
    (cond ((or (snull? s1) (snull? s2)) snull)
          ((= (scar s1) (scar s2)) (scons (scar s1)
                                          (sintersect (scdr s1) s2)))
          ((< (scar s1) (scar s2)) (sintersect (scdr s1) s2))
          (else (sintersect s1 (scdr s2))))))
    
(define multiples-of-6
  (sintersect multiples-of-2 multiples-of-3))

;; Demonstrate that your answer is correct.

(check-equal? (sfirst 10 multiples-of-6) '(6 12 18 24 30 36 42 48 54 60))
(check-equal? (sfirst 2 (sintersect (scons 1 (scons 2 (scons 3 snull)))
                                    (scons 1 (scons 3 (scons 5 snull))))) '(1 3))

;; EXERCISE 4
;;
;; We can do some other interesting things with streams.  For example,
;; we can generate a stream of random numbers:
;;
(define random-stream
  (lambda (n)
    (scons (random n) (random-stream n))))

(define rands (random-stream 100))
;;
;; a. Describe the output of (sfirst rands 10)
;;

;; (sfirst 10 rands) returns (for me, this time) '(19 27 81 50 61 26 46 85 26 45),
;; a stream of 10 apparently random numbers. If we evaluate (sfirst 10 rands) 
;; again, we get '(19 64 75 25 65 93 49 82 50 8), a different random stream but with
;; the same first number. The first number is set when we define rands but the
;; other numbers in the stream are evalutated for a new random value every time
;; we evaluate (sfirst 10 rands).

;; b. What do you expect you would get if you 
;; evaluated run (sfirst rands 10) a second time
;; immediately after evaluating it the first time?
;;

;; If we re-run the whole program, (sfirst 10 rands) returns (for me, this time)
;; '(36 78 6 82 79 31 48 26 17 83). Each time the program is run, the first number
;; in rands is redefined as a random number at (define rands).

;; Note: On Thursday in class, we will look at more efficient ways to
;; store streams so that values in the stream are cached once they are
;; examined.  You may want to come back to this question once you've
;; updated your stream to see how the result you get in 4(b) is
;; different, if at all.


;; EXERCISE 5
;; We can also do some other interesting things, such as generate a
;; stream with alternating true and false values:

(define alternating-booleans
  (scons #t (smap not alternating-booleans)))

;; a. Write the procedure stream-select which takes two streams,
;; input-stream and boolean-stream, and as output, produces a new
;; stream with the following property: position j in the input-stream
;; is included in the output-stream if position j in the
;; boolean-stream is true.
;;
;; For example, if we used positives as the input-stream and
;; alternating-booleans as the boolean-stream, we would get the odd
;; numbers as the output-stream.

(define stream-select
  (lambda (input-stream boolean-stream)
    (cond ((or (snull? input-stream) (snull? boolean-stream)) snull)
          ((scar boolean-stream) (scons (scar input-stream)
                                        (stream-select (scdr input-stream)
                                                       (scdr boolean-stream))))
          (else (stream-select (scdr input-stream)
                               (scdr boolean-stream))))))

;(stream-select positives alternating-booleans) ; => odd positives

;; b. Before you run it, can you figure out what this does:

;(stream-select positives 
;               (stream-select alternating-booleans
;                              alternating-booleans))

;; (stream-select alternating-booleans alternating-booleans) returns a stream
;; with all values #t, since the only values that are included are those that
;; align with #t, which are all the #t. The result is the original stream positives.

;; c. What about this?

;(stream-select positives 
;               (stream-select alternating-booleans
;                              (scdr alternating-booleans)))

;; (stream-select alternating-booleans (scdr alternating-booleans)) returns a
;; stream with all values #f, since the only values that are included are those
;; that align with #t, which are the offset #f. The result is an empty stream.
;; (However, since both streams are infinite, evaluating it never ends.)




;; EXERCISE 6

;; In class we wrote sfilter, which takes a predicate, pred?, and a
;; stream, s, and creates a new stream whose values are only those
;; values of the stream, s, which *satisfy* the predicate:

(define sfilter 
  (lambda (pred? s) 
    (cond ((snull? s) snull) 
          ((pred? (scar s)) (scons (scar s) (sfilter pred? (scdr s)))) 
          (else  
           (sfilter pred? (scdr s)))))) 
 
;; Write a procedure called sfilter-out which creates a new stream
;; whose values are only those values of the stream, s, which *do not
;; satisfy* the predicate, pred?

(define sfilter-out
  (lambda (pred? s)
    (cond ((snull? s) snull)
          ((pred? (scar s)) (sfilter-out pred? (scdr s)))
          (else (scons (scar s) (sfilter-out pred? (scdr s)))))))


;; EXERCISE 7

;; There's not much to do in this exercise except see couple of fun
;; examples, so just play along...

;; Sometimes streams are "divergent streams".
;; For example:
;;
;; First, compute the even positive numbers

(define even (sfilter even? positives))

;; Next, sfilter the stream, keeping only the odds.

;; (define odd (sfilter odd? even))
;;
;; 7a. Without running the above line, describe the stream "odd"
;; below:

;; The stream odd is empty, but it's not snull since it never finishes
;; evalutating. The stream even is infinite, so odd keeps looking for
;; an odd value even though we know there aren't any.


;; 7b. What's in the stream we get by intersecting the negatives and
;; the positives?
;;
;; (define intersection (sintersect negatives positives))

;; The intersection of negatives and positives is like the odd values in
;; the even stream. It's not snull even though we know it will never have
;; any values; it's continuously evaluating infinite streams that don't
;; intersect.


;; EXERCISE 8

;; In Section 3.5.3, there's a discussion of "infinite streams of pairs"
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-24.html#%_sec_Temp_476

;; The text defines a procedure called 'pairs' which uses the 'szip' procedure
;; you wrote in Exercise 1. (The text calls the szip procedure 'interleave'.)
;; If you 'szip' procedure doesn't work, you won't be able to complete this question.
 

(define pairs
  (lambda (s1 s2)
    (scons
     (list (scar s1) (scar s2))
     (szip
      (smap (lambda (x) (list (scar s1) x))
            (scdr s2))
      (pairs (scdr s1) (scdr s2))))))
  
 (define pos-pos (pairs positives positives))

;; (Adapted from Ex. 3.66 in the textbook) Examine the stream pos-pos
;; above. Can you make any general comments about the order in which
;; the pairs are placed into the stream? For example, about how many
;; pairs precede the pair (1,100)? the pair (99,100)? the pair
;; (100,100)?  This question is about thinking how the list of pairs
;; is constructed.  You don't need to get hung up on a mathematical
;; definition of the order of the pairs, though you are welcome to do so!
 
;; Every other pair begins with 1, every other pair of the other pairs
;; begins with 2, every other pair of the other other pairs begins with 3,
;; and so on... So in the first n pairs, n/2 begin with 1, n/4 begin with 2,
;; n/8 begin with 3, ...
;; The pair (1,100) is preceded by 99 other pairs beginning with 1, which
;; are half of the total preceding pairs. So (1,100) is preceded by about
;; 200 pairs (198 pairs).
;; The pair (99,100) is preceded by 99 other pairs beginning with 99, 
;; which are n/(2^99) of the total preceding pairs. So (99,100) is
;; preceded by about 99*(2^99) pairs (overestimate).
;; The pair (100,100) is preceded by 99 other pairs beginning with 100,
;; which are n/(2^100) of the total preceding pairs. So (99,100) is
;; preceded by about 99*(2^100) pairs (overestimate).
