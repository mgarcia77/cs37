#lang racket

#|
Section 3.3.4: A Simulator for Digital Circuits

This simulator will require access to a number of data structures that
I have provided for you. I will discuss them as we need them.

To complete this lab, you do not need to examine any of the supporting
files. However, the full code for the data structures used in this lab
can be found in:
|#
(require "include/objects.rkt")
(require "include/agenda.rkt")
(require "include/wires.rkt")
#|
"include/queue.rkt" is not required here, but is required by agenda.rkt

The book presents the digital circuit simulator in a top-down fashion,
which makes it difficult to test the code as you go along.  Instead, I
have provided you the code in a bottom-up fashion, and have rewritten
the book's code so that it follows the object-oriented style we are
developing in class.

To implement our simulator, we will create an object called an agenda.
An agenda holds two pieces of information: the current time, and a
list of "time segments".

A "time segment" is a queue associated with a particular time.  You
can think of a time segment as a list of things to do at a particular
time. For example, one could imagine a time segment such as

12:30pm "Eat lunch", "Consume caffeine".  

The time segment would be stored in your agenda along with other time
segments such as

4:10pm "Leave lab", "Get more caffeine".

When the time becomes 4:10pm, you would leave lab and go get more
caffeine.

Each of our "to do" items will be a function that will execute at
the specified time.

The agenda supports the following messages:

1. insert! <at-time> <procedure>: inserts <procedure> in the agenda
and associates that item with <at-time>.  (e.g. "At 4:10, leave lab")

2. insert-after-delay! <delay> <procedure>: insert <procedure> after a
<delay> from the current time.  (e.g. "In 5 minutes, remind me to save
what I've done so far.")

3. propagate!: perform all of the actions in the agenda in order,
updating the current time as it executes each item, stopping when
there are no more actions to perform.  (In plain English, this is
something like: "You plan your whole day using insert! and
insert-after-delay! then you just sit back and watch as all the things
you were planning on doing get done in the order you specified. Once
you've completed all of your to-do items, time just magically stops.")

4. get-time: return the current time

To see how the agenda works, let's make a new agenda and try it out.
Remember that the agenda requires our "to do" items to be procedures.
To help visualize the agenda working, I have defined a procedure "md"
of one argument "item" which returns a procedure that displays "item".
|#

(define md  ; md is short for "make-displayer"
  (lambda (item)
    (lambda () (display item) (display " "))))

#|
Now, let's make an agenda and insert some actions we want to perform,
along with when we want to perform them:
|#

(define agendaA (make-agenda))
(ask agendaA 'insert! 2 (md 'm)) ; At time 2, display 'm
(ask agendaA 'insert! 2 (md 'u)) ; At time 2, also display 'u
(ask agendaA 'insert! 5 (md 'o)) ; At time 5, display 'o
(ask agendaA 'insert! 5 (md 'r)) ; At time 5, also display 'r
(ask agendaA 'insert! 3 (md 'l)) ; etc.
(ask agendaA 'insert! 3 (md 'a))
(ask agendaA 'insert! 1 (md 's))
(ask agendaA 'insert! 1 (md 'i))

; display 't 4 time units from now. Since it's currently time 0, this
; inserts the display event at time 4.
(ask agendaA 'insert-after-delay! 4 (md 't)) 

#|
We can't really "look" at the agenda, because all of the items in the
agenda are just procedures.  However, we can perform all of the
actions in the agenda in order, updating the current time as it
executes each item.
|#

(ask agendaA 'propagate!) ;; displays items above in order
(newline)                 ;; print a newline to keep things neater

;; After executing all of the items in the agenda, the time is now 5.
(display "The current time in the agenda is ")
(ask agendaA 'get-time) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Now we have the tools we need to perform actions in order according to
a specified time of execution. We will put this tools to use by
simulating digital circuits.

Note that the book's implementation requires a global "agenda"
variable to be used for the circuit simulation.  Though I could fix
this, the code would be harder to read, so I have left it as it is.
|#

(define agenda (make-agenda))

#|
A wire represents an electrical wire carrying either 0 or 1.  We can
set or get its value.  When we change a wire's value, we perform any
actions associated with that wire. When a wire is created, it's
default signal is 0.

The wire supports the following messages:

1. get-signal: get the current value on the wire

2. set-signal! <value>: changes the value of the wire to <value> and
performs any actions necessary in the circuit connected to this wire.

3. add-action! <procedure>: when the value of the wire changes (from 0
to 1 or from 1 to 0), this <procedure> will be called.

Now we can build our first simple circuit using an inverter.  Signals
through inverters (and other gates) do not travel instantly.  There
are delays, such as shown below, for the signal to propagate through
the gate.

You can read more about progation delay here:
      http://en.wikipedia.org/wiki/Propagation_delay
|#

(define inverter-delay 2)  ; 2 nanoseconds, perhaps?
(define and-gate-delay 3)  ; the actual units don't really matter.

#|
The inverter procedure below defines an internal procedure
invert-input.  When executed, invert-input will tell the agenda to
flip the value on the output wire after "inverter-delay" amount of
time has elapsed.  Be sure you understand how this procedure and the
logical-not procedure are working.
|#

(define inverter
  (lambda (input-wire output-wire)
    (define inverter-action
      (lambda ()
        (let ((new-value (logical-not (ask input-wire 'get-signal))))
          (ask agenda 'insert-after-delay! inverter-delay 
               (lambda () (ask output-wire 'set-signal! new-value))))))
    (ask input-wire 'add-action! inverter-action)))

(define logical-not
  (lambda (value)
    (if (= value 0) 1 0)))

#|
A probe prints out the value on a wire anytime its value changes.
(You may wish to consult Racket's "Help Desk" about printf if you are
curious about how it works in general, though it should be clear how
it works in this example.)
|#

(define probe
  (lambda (name wire)
    (define probe-action
      (lambda ()
        (printf "@time ~v, wire ~v = ~v\n"
                (ask agenda 'get-time)
                name
                (ask wire 'get-signal))))
    (ask wire 'add-action! probe-action)))

;; Let's test this by making two wires, a and b, and connecting them
;; with an inverter where a is the input and b is the output.

(newline)
(display "TESTING INVERTER\n")

(define wireA (make-wire))
(define wireB (make-wire))
(inverter wireA wireB)

;; Then, let's probe b; by default it starts at 0.
(display "Hooking up the probe to B.\n")
(probe "B" wireB)

;; After the inverter delay, b should become 1.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; Then, let's set a to be 1.
(display "Changing A to 1.\n")
(ask wireA 'set-signal! 1)

;; After the inverter delay, b should become 0.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Exercise 1.  Write the implementation of "logical-and". 

Below is the definition of an and-gate adapted from SICP:
http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_idx_3382

It will not work until you write "logical-and" below.
|#

(define and-gate
  (lambda (in1 in2 out)
    (define and-action-procedure
      (lambda ()
        (let ((new-value
               (logical-and (ask in1 'get-signal)
                            (ask in2 'get-signal))))
          (ask agenda 'insert-after-delay! and-gate-delay
               (lambda () (ask out 'set-signal! new-value))))))
    (ask in1 'add-action! and-action-procedure)
    (ask in2 'add-action! and-action-procedure)))

#|
a. Implement "logical-and". See "logical-not" above for an example of
what to do here.
|#

(define logical-and
  (lambda (value1 value2)
    (if (and (= value1 1) (= value2 1)) 1 0)))

#|
b. Test your and-gate by hooking up three wires (two in, one out) to
an and-gate, probing the output wire, setting the values on the input
wires, and finally propagating the agenda.
|#

(newline)
(display "TESTING AND-GATE\n")
(display "Start time: ")
(ask agenda 'get-time)

(define wireX (make-wire))
(define wireY (make-wire))
(define wireZ (make-wire))

(and-gate wireX wireY wireZ)

;; probe z
(display "Hooking up the probe to Z.\n")
(probe "Z" wireZ)

;; z should still be 0.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; Then, let's set x to be 1.
(display "Changing X to 1.\n")
(ask wireX 'set-signal! 1)

;; After the and-gate delay, z should still be 0.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; Then set y to be 1.
(display "Changing Y to be 1.\n")
(ask wireY 'set-signal! 1)

;; After the and-gate delay, z should become 1.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; time should be +9 from start time
;; (num propagate * and-gate-delay = 3 * 3)
(display "End time: ")
(ask agenda 'get-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Exercise 2. Implement "or-gate" and "logical-or".

a. In order to move on to Exercise 3, you will need to implement an
"or-gate", which should be fairly similar to the implementation of an
"and-gate" above.  You will also need to implement "logical-or".
|#

(define or-gate-delay 5) 

(define or-gate
  (lambda (in1 in2 out)
    (define or-action-procedure
      (lambda ()
        (let ((new-value
               (logical-or (ask in1 'get-signal)
                           (ask in2 'get-signal))))
          (ask agenda 'insert-after-delay! or-gate-delay
               (lambda () (ask out 'set-signal! new-value))))))
    (ask in1 'add-action! or-action-procedure)
    (ask in2 'add-action! or-action-procedure)))

(define logical-or
  (lambda (value1 value2)
    (if (or (= value1 1) (= value2 1)) 1 0)))

#|
b. Test your or-gate by hooking up three wires (two in, one out) to an
or-gate, probing the output wire, setting the values on the input
wires, and finally propagating the agenda.
|#


(newline)
(display "TESTING OR-GATE\n")
(display "Start time: ")
(ask agenda 'get-time)

(define wireJ (make-wire))
(define wireK (make-wire))
(define wireL (make-wire))

(or-gate wireJ wireK wireL)

;; probe l
(display "Hooking up the probe to L.\n")
(probe "L" wireL)

;; l should still be 0.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; Then, let's set j to be 1.
(display "Changing j to 1.\n")
(ask wireJ 'set-signal! 1)

;; After the or-gate delay, l should become 1.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; Then set k to be 1.
(display "Changing K to be 1.\n")
(ask wireK 'set-signal! 1)

;; l should still be 1.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; set j back to 0
(display "Changing J back to 0.\n")
(ask wireJ 'set-signal! 0)

;; l should still be 1
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; time should be +20 from last time display
;; (num propagate * or-gate-delay = 4 * 5)
(display "End time: ")
(ask agenda 'get-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| 
Exercise 3 (adapted from Exercise 3.29 from the text)

Here is the definition of the half-adder adapted from SICP.  The
variables a, b, s, and c correspond to Figure 3.25:
http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_fig_3.25
|#

;; UNCOMMENT half-adder WHEN YOU GET TO THIS QUESTION 

(define half-adder
  (lambda (a b s c)
    (let ((d (make-wire))
          (e (make-wire)))
      (or-gate a b d)
      (and-gate a b c)
      (inverter c e)
      (and-gate d e s))))


#|
Another way to construct an or-gate is as a compound digital logic
device, built from and-gates and inverters.

If you are unsure how to do this, De Morgan's Laws might help:
http://en.wikipedia.org/wiki/De_Morgan's_laws

a. Define the procedure compound-or-gate that accomplishes this. The
half-adder circuit above is a good example of how to create a compound
circuit.
|#

;; x|y is #t when ~(~x & ~y) is #f

(define compound-or-gate
  (lambda (in1 in2 out) 
    (let ((a (make-wire))
          (b (make-wire))
          (c (make-wire)))
      (inverter in1 a)
      (inverter in2 b)
      (and-gate a b c)
      (inverter c out))))
     
#|
b. What is the delay time of the compound-or-gate in terms of
and-gate-delay and inverter-delay?
|#

;; The total delay time is one inverter plus one and-gate plus
;; another inverter. The first two inverters (invert in1 and
;; invert in2) can run in parallel, but then the rest of
;; the gates must all run sequentially.
;; Note also that if one input is changed but the output of
;; the and-gate remains the same, then the delay is only the
;; time of the first inverter and the and-gate, since the final
;; inverter doesn't need to recalculate.

#|
c. Test this by hooking up three wires (two in, one out) to a
compound-or-gate, probing the output wire, setting the values on the
input wires, and finally propagating the agenda.
|#

(newline)
(display "TESTING COMPOUND-OR-GATE\n")
(display "Start time: ")
(ask agenda 'get-time)

(define wireP (make-wire))
(define wireQ (make-wire))
(define wireR (make-wire))

(compound-or-gate wireP wireQ wireR)

;; probe l
(display "Hooking up the probe to R.\n")
(probe "R" wireR)

;; r should still be 0.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; Then, let's set p to be 1.
(display "Changing p to 1.\n")
(ask wireP 'set-signal! 1)

;; After the compound-or-gate delay, r should become 1.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; Then set q to be 1.
(display "Changing Q to be 1.\n")
(ask wireQ 'set-signal! 1)

;; r should still be 1.
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; set p back to 0
(display "Changing P back to 0.\n")
(ask wireP 'set-signal! 0)

;; r should still be 1
(display "Propagating the agenda.\n")
(ask agenda 'propagate!)

;; time is be +24 from last time display
;; 2 * (2 * inverter + and-gate) + 3 * (1 * inverter + and-gate)
;;    = 2 * (2 * 2 + 3) + 2 * (2 + 3) = 14 + 10
(display "End time: ")
(ask agenda 'get-time)

#|
d. Did you notice anything strange about the probed value of the
output? Why do you think that happened? 
|#

;; When the agenda propagates, the probe on R may have multiple
;; messages. This is because the various delays in the compound-
;; or-gate need to catch up to each other. Until all the gates have
;; been computed, the output will reflect the current state of the
;; or-gate, which is not necessarily the final state of the gate.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Exercise 4. (Adapted from Exercise 3.30 in the text). See details:
http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_thm_3.30
|#

#|
a. The definition for the full-adder is adapted from the text below.
Test the full-adder by hooking up appropriate input and output wires,
propagating the agenda, then asking the output wires for their values.
Be sure you understand how the full-adder works.
|#

;; UNCOMMENT full-adder WHEN YOU GET TO THIS QUESTION 

(define full-adder
  (lambda (a b c-in sum c-out)
    (let ((s (make-wire))
          (c1 (make-wire))
          (c2 (make-wire)))
      (half-adder b c-in s c1)
      (half-adder a s sum c2)
      (or-gate c1 c2 c-out))))


(newline)
(display "TESTING FULL ADDER\n")
(display "Start time: ")
(ask agenda 'get-time)

(define wireM (make-wire))
(define wireN (make-wire))
(define cIn (make-wire))
(define cOut (make-wire))
(define wireSum (make-wire))

(full-adder wireM wireN cIn wireSum cOut)

(display "a = ")
(ask wireM 'get-signal)
(display "b = ")
(ask wireN 'get-signal)
(display "c-in = ")
(ask cIn 'get-signal)
(display "Propagating the agenda\n")
(ask agenda 'propagate!)
(display "sum = ")
(ask wireSum 'get-signal)
(display "c-out = ")
(ask cOut 'get-signal)

(newline)

(ask wireM 'set-signal! 1)
(display "a = ")
(ask wireM 'get-signal)
(display "b = ")
(ask wireN 'get-signal)
(display "c-in = ")
(ask cIn 'get-signal)
(display "Propagating the agenda\n")
(ask agenda 'propagate!)
(display "sum = ")
(ask wireSum 'get-signal)
(display "c-out = ")
(ask cOut 'get-signal)

(newline)

(ask wireN 'set-signal! 1)
(display "a = ")
(ask wireM 'get-signal)
(display "b = ")
(ask wireN 'get-signal)
(display "c-in = ")
(ask cIn 'get-signal)
(display "Propagating the agenda...\n")
(ask agenda 'propagate!)
(display "sum = ")
(ask wireSum 'get-signal)
(display "c-out = ")
(ask cOut 'get-signal)

(newline)

(ask cIn 'set-signal! 1)
(display "a = ")
(ask wireM 'get-signal)
(display "b = ")
(ask wireN 'get-signal)
(display "c-in = ")
(ask cIn 'get-signal)
(display "Propagating the agenda...\n")
(ask agenda 'propagate!)
(display "sum = ")
(ask wireSum 'get-signal)
(display "c-out = ")
(ask cOut 'get-signal)

(newline)

(ask wireM 'set-signal! 0)
(display "a = ")
(ask wireM 'get-signal)
(display "b = ")
(ask wireN 'get-signal)
(display "c-in = ")
(ask cIn 'get-signal)
(display "Propagating the agenda...\n")
(ask agenda 'propagate!)
(display "sum = ")
(ask wireSum 'get-signal)
(display "c-out = ")
(ask cOut 'get-signal)


#|
b. Write a procedure ripple-carry-adder. The circuit for the ripple
carry adder is shown in Figure 3.27:

http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_fig_3.27

You can assume the procedure receives an equal number of wires in A,
B, and S. You can also assume each list contains at least one wire.
(This will make sense once you've read the question in the text.)
|#

(define ripple-carry-adder
  (lambda (A B S c)
    (let ((c-out (make-wire)))
      (cond ((null? (cdr A)) (full-adder (car A) (car B) (make-wire) (car S) c))
            (else (ripple-carry-adder (cdr A) (cdr B) (cdr S) c-out)
                  (full-adder (car A) (car B) c-out (car S) c))))))
     

(newline)
(display "RIPPLE CARRY ADDER\n")

(define a (make-wire))
(ask a 'set-signal! 1)
(define b (make-wire))
(ask b 'set-signal! 1)
(define c (make-wire))
(ask c 'set-signal! 1)
(define d (make-wire))
(ask d 'set-signal! 1)
(define e (make-wire))
(define f (make-wire))

(define aWire (list a b))
(define bWire (list c d))
(define sWire (list e f))
(define c-out (make-wire))
(define adder (ripple-carry-adder aWire bWire sWire c-out))

(probe "E" e)
(probe "F" f)
(ask agenda 'propagate!)

(display "Result: \n")
(ask e 'get-signal)
(ask f 'get-signal)
(ask c-out 'get-signal)

#|
c. What is the delay needed to obtain the complete output from an
n-bit ripple-carry adder, expressed in terms of the delays for
and-gates, or-gates, and inverters?
|#

;; An n-bit ripple-carry-adder needs the delay for the nth adder
;; plus the delay for the n-1 other adders. The nth full-adder
;; needs two half-adders and an or-gate. Each of the half-adders
;; needs one or-gate, one inverter, and two and-gates, arranged
;; such that the longest path takes 8 units of time (either one
;; or-gate plus one and-gate, or two and-gates plus one inverter).
;; So one full-adder takes 2(or-gate + and-gate) + or-gate, equal
;; to 3 or-gates + 2 and-gates. The whole n-bit adder takes n
;; full-adders, or n(3 * or-gate + 2 * and-gate).

#|
Complete part d. -or- the CHALLENGE question in part e. below.

d. Demonstrate that your ripple-carry-adder works by creating an 8-bit
ripple-carry-adder which adds together the decimal values of 47 and
101.  You will need to convert these to binary, set up the wires
properly, and then display the answer (in 8-bit binary). Convert your
answer back to decimal to be sure that it equals 148.  CHALLENGE
question e. make this work easily for any two positive integers.
|#

;;  47 = 0010 1111
;; 101 = 0110 0101
;;     + 1001 0100 = 148

(define a1 (make-wire))
;(ask a1 'set-signal! 1)
(define a2 (make-wire))
;(ask a2 'set-signal! 1)
(define a3 (make-wire))
(ask a3 'set-signal! 1)
(define a4 (make-wire))
;(ask a4 'set-signal! 1)
(define a5 (make-wire))
(ask a5 'set-signal! 1)
(define a6 (make-wire))
(ask a6 'set-signal! 1)
(define a7 (make-wire))
(ask a7 'set-signal! 1)
(define a8 (make-wire))
(ask a8 'set-signal! 1)

(define b1 (make-wire))
;(ask b1 'set-signal! 1)
(define b2 (make-wire))
(ask b2 'set-signal! 1)
(define b3 (make-wire))
(ask b3 'set-signal! 1)
(define b4 (make-wire))
;(ask b4 'set-signal! 1)
(define b5 (make-wire))
;(ask b5 'set-signal! 1)
(define b6 (make-wire))
(ask b6 'set-signal! 1)
(define b7 (make-wire))
;(ask b7 'set-signal! 1)
(define b8 (make-wire))
(ask b8 'set-signal! 1)

(define s1 (make-wire))
(define s2 (make-wire))
(define s3 (make-wire))
(define s4 (make-wire))
(define s5 (make-wire))
(define s6 (make-wire))
(define s7 (make-wire))
(define s8 (make-wire))

(define A (list a1 a2 a3 a4 a5 a6 a7 a8))
(define B (list b1 b2 b3 b4 b5 b6 b7 b8))
(define S (list s1 s2 s3 s4 s5 s6 s7 s8))

(newline)
(display "8-BIT ADDER:\n")
(ripple-carry-adder A B S c-out)
(ask agenda 'propagate!)
(ask s1 'get-signal)
(ask s2 'get-signal)
(ask s3 'get-signal)
(ask s4 'get-signal)
(ask s5 'get-signal)
(ask s6 'get-signal)
(ask s7 'get-signal)
(ask s8 'get-signal)

#|
e. CHALLENGE: First, write a procedure called setup-wires that takes
two parameters, a decimal number and a number of bits, and produces a
list of wires initialized to the value of the decimal number. You
should assume that the decimal number can fit in the number of bits
specified, but if you don't want to assume that, simply return the
lowest n bits that fit.

Once you've computed the answer with the ripple-carry-adder, convert
the answer contained on S and carry-out back to decimal using a
function called ripple-to-decimal.

If you do this question, you should be able to answer question d.
like this:
|#

;; Uncomment this if you do the challenge quesiton.

(define rippler
  (lambda (x y)
    (define bits-needed
      (lambda (n)
        (inexact->exact (ceiling (/ (log (add1 n)) (log 2))))))
    (let ((bits (bits-needed (max x y))))
      (let ((A (setup-wires-decimal x bits))
            (B (setup-wires-decimal y bits))
            (S (setup-wires-decimal 0 bits))
            (carry-out (make-wire)))
        (ripple-carry-adder A B S carry-out)
        (ask agenda 'propagate!)
        (ripple-to-decimal S carry-out)))))

(define setup-wires-decimal
  (lambda (x bits)
    (let ((a (make-wire)))
      (cond ((= bits 0) '())
            ((>= x (expt 2 (- bits 1))) (ask a 'set-signal! 1)
                                       (cons a (setup-wires-decimal (- x (expt 2 (- bits 1))) (- bits 1))))
            (else (cons a (setup-wires-decimal x (- bits 1))))))))


(define ripple-to-decimal
  (lambda (S c-out)
    (define calc-sum
      (lambda (S)
        (cond ((null? S) 0)
              ((= 1 (ask (car S) 'get-signal)) (+ (expt 2 (- (length S) 1))
                                                  (calc-sum (cdr S))))
              (else (calc-sum (cdr S))))))
    (+ (calc-sum S) (* (ask c-out 'get-signal) (expt 2(length S))))))


(require rackunit)
(check-equal? (rippler 47 101) (+ 47 101))
(check-equal? (rippler 40327 25210) (+ 40327 25210))