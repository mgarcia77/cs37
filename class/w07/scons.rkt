#lang racket

(require racket/trace)


(define-syntax-rule (delay-expression expr)
  (lambda () expr))

(define force-expression
  (lambda (expr)
    (expr)))


(define-syntax-rule (scons a d)
  (mcons a (delay-expression d)))

(define scar mcar)
(define scdr
  (lambda (s)
    (force-expression (mcdr s))))
(define snull null)
(define snull? null?)


(define plus1 (lambda (n) (add1 n)))  ;; wrapper to allow trace
(trace plus1)

(define senumerate
  (lambda (low high)
    (cond ((> low high) snull)
          (else (scons low (senumerate (add1 low) high))))))  ;; scons delays calling senumerate recursively

;; returns list of first n elts of stream
(define sfirst
  (lambda (n stream)
    (cond ((<= n 0) snull)
          ((snull? stream) snull)
          (else (cons (scar stream) (sfirst (sub1 n) (scdr stream)))))))

(define biglist (senumerate 1 1000000000))  ;; don't calculate beyond first pair until later elt is asked for
(sfirst 10 biglist)  ;; recalculates on each call


(define smap
  (lambda (f s)
    (cond ((snull? s) snull)
          (else (scons (f (scar s)) (smap f (scdr s)))))))

(smap plus1 biglist)

(define stream-combiner
  (lambda (f)
    (lambda (s1 s2)
      (cond ((or (snull? s1) (snull? s2)) snull)
            (else (scons (f (scar s1) (scar s2))
                         ((stream-combiner f) (scdr s1) (scdr s2))))))))

(define stream1 (senumerate 1 10))
(define stream2 (senumerate 21 30))
;(define stream3 (stream-combiner + stream1 stream2))

;(define add-streams (lambda (s1 s2) (stream-combiner + s1 s2)))
(define add-streams (stream-combiner +))
(define mult-streams (stream-combiner *))

(define stream3 (add-streams stream1 stream2))

(define squares (mult-streams (senumerate 1 100) (senumerate 1 100)))

(define f (scons 0 (senumerate 1 100)))

; w is infinite: contains all positive ints
(define whole (scons 0 (smap add1 whole)))  ;; can refer to undefined w in definition because not evaluated yet
                                    ;; (w will be defined by the time (smap add1 w) gets evaluated)
(define positives (scdr whole))
(define negatives (smap - positives))

(define mul-3 (smap (lambda (n) (* n 3)) positives))
(define mul-2 (smap (lambda (n) (* n 2)) positives))
(define mul-5 (add-streams mul-3 mul-2))

(define factorials (scons 1 (mult-streams (scdr positives) factorials)))

(define fibonacci (scons 1 (scons 1 (add-streams fibonacci (scdr fibonacci)))))

(define sfoldl
  (lambda (proc base stream)
    (cond ((snull? stream) base)
          (else (proc (scar stream) (sfoldl proc base (scdr stream)))))))

(define sfilter
  (lambda (pred? s)
    (cond ((snull? s) snull)
          ((pred? (scar s)) (scons (scar s) (sfilter pred? (scdr s))))
          (else (sfilter pred? (scdr s))))))