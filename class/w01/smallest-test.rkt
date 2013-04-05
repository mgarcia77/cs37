#lang racket

(require rackunit "smallest.rkt")

(check-equal? (find-smallest 1 1 1) 1)
(check-equal? (find-smallest 2 2 4) 2)
(check-equal? (find-smallest 2 4 2) 2)
(check-equal? (find-smallest 4 2 2) 2)
(check-equal? (find-smallest 3 4 4) 3)
(check-equal? (find-smallest 4 3 4) 3)
(check-equal? (find-smallest 4 4 3) 3)
(check-equal? (find-smallest 5 6 7) 5)
(check-equal? (find-smallest 6 5 7) 5)
(check-equal? (find-smallest 5 7 6) 5) 

; an intentionally bad test
; (check-equal? (find-smallest 5 7 6) 4) 
