#lang racket

(require "bin-freq-tree.rkt")
(require "priority-queue.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Creating codes

Fixed length     
ASCII uses 7 bits per character so there are 2^7 possible characters
which is 128.

Variable length
Morse uses dots and dashes and creates shorter patterns for more
frequent characters.

A problem with using variable length codes is knowing where one code
stops and the next one starts.  Morse uses a pause to separate codes. 

Another solutions is to design a code such that no complete code for
any symbol is the prefix of the code for another symbol.  Such a code
is called a prefix code.

For example, in the following encoding, the codes for A and B are not
the prefix of any other code.  

A 0
B 100
C 1010
D 1011
E 1100
F 1101
G 1110
H 1111

In general we can attain significant savings if we use variable length
prefix code.  David Huffman invented such an encoding method.  

In a Huffman encoding, the code can be represented as a binary tree
whose leaves are the symbols encoded and whose internal nodes are sets
including all of the symbols at the leaves below as well as the sum of
their frequencies.

Suppose I have the message: 

test test this is a test this is only a test

1. Calculate frequencies of symbols 

((a 2) (is 2) (only 1) (test 4) (this 2))

2. Put them into a priority queue from lowest to highest frequency.

((only 1) (a 2) (is 2) (this 2) (test 4))

3. Merge lowest frequency nodes into a new tree and then put that tree
   back into the priority queue.

((is 2) (this 2) ({only a} 3) (test 4))

(({only a} 3) ({is this} 4) (test 4))

((test 4) ({only a is this} 7))

({test only a is this} 11)

{test only a is this} 11
   /           \
  /             \
test 4   {only a is this} 7
           /         \
          /           \
     {only a} 3   {is this} 4
        /    \      /    \
       /      \    /      \
     only 1  a 2  is 2  this 2

test test this is a test this is only a test

test = 0
only = 100
a    = 101
is   = 110
this = 111

So the encoding of our message would be:

0011111010101111101001010 
length 24

To represent 5 symbols in a fixed length code would take 3 bits and
our message was 11 symbols long, for a total length of 33. So the
Huffman encoding was more efficient in this case.

Decoding messages

We simply start at the root of the Huffman tree, then go down to the
left for a 0 and to the right for a 1.  When we reach a leaf we output
the symbol there and then return to the root of the tree to continue
decoding.  

only test this

1000111

|#

;; input: a list of symbols
;; output: an unordered list of (symbol count) pairs counting the number
;;         of symbols in the input
(define symbol-counter
  (lambda (sym-list)
    (define insert-symbol
      (lambda (symbol counts)
        (cond ((null? counts) (list (cons symbol 1)))
              ((eq? symbol (caar counts))
               (cons (cons (caar counts) (+ (cdar counts) 1))
                     (cdr counts)))
              (else
               (cons (car counts) (insert-symbol symbol (cdr counts)))))))
    (define helper
      (lambda (sym-list counter)
        (cond ((null? sym-list) counter)
              (else (helper (cdr sym-list) 
                            (insert-symbol (car sym-list) counter))))))
    (helper sym-list null)))

;; input: two binary frequency trees 
;; output: #t if the first frequency tree has lower frequency; #f otherwise
(define bintree-freq<
  (lambda (bt1 bt2)
    (< (node-freq (get-root bt1)) (node-freq (get-root bt2)))))


;; input: a priority queue storing frequency trees
;; output: a priority queue with the first two elements of the
;;         queue merged together and then inserted into the
;;         priority queue (in order)
(define merge-smallest
  (lambda (pq)
    (cond ((pqueue-empty? pq) pq)
          ((pqueue-empty? (pqueue-rest pq)) pq)
          (else
           (let ((first (pqueue-head pq))
                 (second (pqueue-head (pqueue-rest pq)))
                 (remainder (pqueue-rest (pqueue-rest pq))))
             (let ((new-node (merge-trees first second)))
               (pqueue-insert new-node remainder)))))))


;; input: a priority queue storing frequency trees
;; output: a priority queue with just one element after all
;;         elements have been recursively merged
(define merge-all
  (lambda (pq)
    (cond ((pqueue-empty? pq) pq)
          ((pqueue-empty? (pqueue-rest pq)) pq)
          (else
           (merge-all (merge-smallest pq))))))
             
;; input: a list of symbols representing the message
;; output: a binary frequency tree built according to the
;;         Huffman algorithm
(define message->tree
  (lambda (message)
    (define symbol-counts->binary-trees
      (lambda (symcounts)
        (cond ((null? symcounts) null)
              (else 
               (cons (make-tree (make-node (list (caar symcounts))
                                           (cdar symcounts)) null null)
                     (symbol-counts->binary-trees (cdr symcounts)))))))
    (pqueue-head
     (merge-all
      (list->pqueue
       (symbol-counts->binary-trees
        (symbol-counter message))
       bintree-freq<)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; You write these
;;
;;

;; input: a symbol and a Huffman tree
;; output: a list of 0's and 1's representing the location of the symbol
;;         in the tree using the Huffman encoding algorithm
(define encode-symbol
  (lambda (symbol tree)
    '...))

;; input: a message and a Huffman tree
;; output: a list of 0's and 1's representing encoding of the entire
;;         message using the Huffman encoding algorithm
;; hint: you may want to use append in your solution
(define encode
  (lambda (message tree)
    '...))

;; input: an encoded message and a Huffman tree
;; output: a list of two elements; the first element is the
;;         first symbol found in the Huffman tree when using
;;         the Huffman decoding algorithm on the encoded message;
;;         the second element is the remainder of the message
;;         left to be decoded.
;; hint: you may want to use this for the decode function.
;;       if you don't want to use it for decode, don't do it.
(define decode-partial
  (lambda (encoded tree)
    '...))

;; input: an encoded message and a Huffman tree
;; output: a list of symbols representing the unencoded message
;; hint: you may want to use the decode-partial function above  
(define decode
  (lambda (encoded tree)
    '...))


;; some variables for testing

(define message '(test test this is a test this is only a test))
(define tree (message->tree message))
(define encoded (encode message tree))
(define decoded (decode encoded tree))
;; note: decoded should now equal message
