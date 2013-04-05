;; Madison Garcia, February 2013

;; EXERCISE 10 HAS NOT BEEN COMPLETED
;; The altered implementations of point and segment seem to be
;; working, but figure is still screwy. The start of what I've done 
;; is commented out in the section with the previous implementation 
;; of make-figure. Figures also seem to be randomly missing segments
;; when they are drawn; it's unpredictable and I'm really not
;; sure anymore what's up with that.

#lang racket

(require graphics/graphics)

;; Graphics and Animation

;;----------------- Preliminary definitions -----------------

(define graphics-window #f)

;; The dimension variable determines the size of the graphics window.
(define dimension 500)

;;------------ Open and close the graphics window ------------

;; Opens a new graphics window for drawing.
(define start-graphics
  (lambda ()
    (open-graphics)
    (set! graphics-window (open-viewport "Graphics" dimension dimension))
    'done))

;; Closes the current graphics window.
(define end-graphics
  (lambda ()
    (close-viewport graphics-window)
    'done))

;;--------------------- Abstraction: Color --------------------
;;
;; These functions are built into the graphics/graphics library included 
;; above.  You do not need to add any code in the Color abstraction.
;;
;; Takes three values in the range 0 (dark) to 1 (bright) and returns
;; an rgb (a color).
;;
;; (make-rgb red green blue) 
;;
;; Return the red, green, and blue components, of a color.
;; (rgb-red color)
;; (rgb-blue color)
;; (rgb-green color)
;;
;; Reports whether v is a color
;; (rgb? v) 

;; Some example colors:
(define black  (make-rgb 0 0 0))
(define white  (make-rgb 1 1 1))
(define red    (make-rgb 1 0 0))
(define green  (make-rgb 0 1 0))
(define blue   (make-rgb 0 0 1))
(define garnet (make-rgb 0.625 0 0))

;; Note: rgb is an example of a Racket structure.  Though we will
;; not be covering structures, I am including a file called 
;; "structures.rkt" in this directory for a little extra reading
;; if you are interested.

;;--------------------- Abstraction: Point --------------------
;;
;; Here is the representation for points that we will use.
;; A point is represented as a list containing the point's
;; x-coordinate and y-coordinate values.

(define make-point
  (lambda (x y)
    (list 'point (cons x y))))

(define x-coord
  (lambda (point)
    (caar point)))

(define y-coord
  (lambda (point)
    (cdar point)))

;;-------------------- Abstraction: Segment --------------------
;;
;; Here is our representation for segments.  A segment is 
;; represented as a list containing the start and end points.
#|
(define make-segment
  (lambda (start end)
    (list start end)))
|#

 (define make-segment
   (lambda (start end . optional-args-list)
     (cond ((null? optional-args-list) (list 'segment (list start end) black)) ;; default to black
           ((rgb? (car optional-args-list)) (list 'segment (list start end) (car optional-args-list))) ;; also default to black
           (else (list 'segment (list start end) black)))))

(define start
  (lambda (segment)
    (cdar (cadr segment))))

(define end
  (lambda (segment)
    (cdadr (cadr segment))))

 (define color-of
   (lambda (segment)
     (caddr segment)))


;; In the graphics/graphics library, the point (0,0) is positioned in
;; the upper left corner of the graphics window.  We would prefer to 
;; have (0,0) be at the center of the window.  The procedure 
;; adjust-coordinates does this conversion.

(define adjust-coordinates
  (lambda (segment)
    (let ((x1 (+ (/ dimension 2) (x-coord (start segment))))
          (y1 (- (/ dimension 2) (y-coord (start segment))))
          (x2 (+ (/ dimension 2) (x-coord (end segment))))
          (y2 (- (/ dimension 2) (y-coord (end segment)))))
      (make-segment (make-point x1 y1) (make-point x2 y2)))))

      
(define draw-segment
  (lambda (segment)
    (let ((adjseg (adjust-coordinates segment)))
      ((draw-line graphics-window) 
       (make-posn (x-coord (start adjseg)) (y-coord (start adjseg)))
       (make-posn (x-coord (end adjseg)) (y-coord (end adjseg)))
       (color-of segment)))))

(define erase-segment
  (lambda (segment)
    (let ((adjseg (adjust-coordinates segment)))
      ((clear-line graphics-window) 
       (make-posn (x-coord (start adjseg)) (y-coord (start adjseg)))
       (make-posn (x-coord (end adjseg)) (y-coord (end adjseg)))))))



;;--------------------- Abstraction: Figures -------------------

;;---------------------- *** EXERCISES *** --------------------

;; ***************************************************************
;; *** Exercise 1 can be found in the README-graphics.rkt file *** 
;; ***************************************************************

;; The 'figure' abstraction is implemented as a list of segments.
;; make-figure takes any number of segments and combines them all into
;; a single figure.  Since combining segments into a list can be done
;; using 'list', we will simply define make-figure to be list:

(define make-figure list)
#|
(define make-figure
  (lambda segments
    (cond ((null? segments) (list 'figure null))
          (else (list 'figure segments)))))
|#
;; To extract the first segment and the rest of the segments, we can use
;; car and cdr:

(define first-segment car)
(define rest-segments cdr)

#|
(define get-segments cdr)
(define first-segment (lambda (shape) (car (get-segments shape))))
(define rest-segments (lambda (shape) (cdr (get-segments shape))))
|#
;; To test if a figure has no more segments, we can use null?
(define empty-figure? null?)
#|
(define empty-figure? 
  (lambda (shape)
    (null? (get-segments shape))))
|#
;; To add a single segment to a figure, we can use cons

(define add-segment cons)
#|
(define add-segment
  (lambda (shape new-seg)
    (cons new-seg (get-segments shape))))
|#
;; combine-figures takes any number of figures and combines them all
;; into a single figure.  Since combining lists into one list can be
;; done using 'append', we will simply define combine-figures to be
;; append:

(define combine-figures append)

#|
(define combine-figures
  (lambda (shape1 shape2)
    (append (get-segments shape1) (get-segments shape2))))
|#

;; Exercise 2
;; draw-figure takes a figure and draws it on the screen.
;; Complete the draw-figure function.

#|
(define draw-figure
  (lambda (figure)
    (cond ((empty-figure? figure) null)
          (else (draw-segment (first-segment figure)) (draw-figure (rest-segments figure))))))
|#


#|
;;TEST: DRAW A SQUARE
(start-graphics)
(draw-figure (make-figure (make-segment (make-point 0 0) (make-point 0 -100))
               (make-segment (make-point 0 -100) (make-point -100 -100))
               (make-segment (make-point -100 -100) (make-point -100 0))
               (make-segment (make-point -100 0) (make-point 0 0))))
|#

;; Exercise 3
;; erase-figure takes a figure and erases it from the screen.  Note
;; that the easy answer is to cut and paste your answer from Exercise
;; 2, substituting draw-segment with erase-segment.  However, a better
;; answer is to design a higher-order function which takes either
;; draw-segment or erase-segment as a parameter along with the figure.
;; Create this higher-order function called place-figure.  Then,
;; redefine draw-figure from Exercise 2, and write erase-figure.

 (define place-figure
  (lambda (figure placement-type)
    (cond ((empty-figure? figure) null)
          (else (placement-type (first-segment figure)) 
                (place-figure (rest-segments figure) placement-type)))))

 (define draw-figure
  (lambda (figure)
    (place-figure figure draw-segment)))

 (define erase-figure
  (lambda (figure)
    (place-figure figure erase-segment)))



;; TEST: DRAW AND ERASE A SQUARE
 (define square (make-figure (make-segment (make-point 0 0) (make-point 0 -100))
                             (make-segment (make-point 0 -100) (make-point -100 -100))
                             (make-segment (make-point -100 -100) (make-point -100 0))
                             (make-segment (make-point -100 0) (make-point 0 0))))
(start-graphics)
(draw-figure square)
(erase-figure square)


;; Exercise 4
;; make-polyline takes a list of points and returns a figure
;; consisting of segments connecting the first point to the
;; second point, the second point to the third point, and
;; so on.  The list is assumed to contain at least two points.
;; Complete the make-polyline function.  
;;
;; **** NOTE: This procedure does NOT draw anything, it 
;;            only returns a new figure. 

 (define make-polyline
   (lambda (points . optional-arg-list)
     (cond ((null? points) null)
           ((null? (cdr points)) null)
           ((null? optional-arg-list) (add-segment (make-segment (car points) (cadr points))
                                                   (make-polyline (cdr points))))
           (else (add-segment (make-segment (car points) (cadr points) (car optional-arg-list))
                              (make-polyline (cdr points) (car optional-arg-list)))))))
 
 

;; DRAW A POLYLINE
 (define point-list (list (make-point 0 0) (make-point 100 100) (make-point 200 -200)))
 (start-graphics)
 (draw-figure (make-polyline point-list))




;; Exercise 5
;; make-polygon takes a list of points and returns a figure in which
;; the points are connected together by segments in a closed circuit.
;; The list is assumed to contain at least two points.
;;
;; **** NOTE: This procedure does NOT draw anything, it 
;;            only returns a new figure. 
;;
;; For example:
;;
;; (define sqr 
;;  (make-polygon
;;   (list
;;     (make-point 0 0)
;;     (make-point 100 0)
;;     (make-point 100 100)
;;     (make-point 0 100))))
;;
;; creates a figure with points (0 0), (100 0), (100 100) and (0 100)
;; named sqr
;;
;; Complete the make-polygon function.

 (define make-polygon
   (lambda (points . optional-arg-list)
     (cond ((null? optional-arg-list) (make-polyline (append points (cons (car points) null))))
           (else (make-polyline (append points (cons (car points) null)) (car optional-arg-list))))))

 
 (define sqr 
  (make-polygon
   (list
     (make-point 0 0)
     (make-point 100 0)
     (make-point 100 100)
     (make-point 0 100))
   blue))
 
 (define triangle
   (make-polygon
    (list
     (make-point 0 0)
     (make-point 100 50)
     (make-point -235 175))
    red))
 
 
;; DRAW BLUE SQUARE AND RED TRIANGLE
 (start-graphics)
 (draw-figure sqr)
 (draw-figure triangle)
 

;; Exercise 6
;; shift-figure takes a figure and a dx and a dy and creates a new
;; figure where every point in the original figure is shifted by
;; these amounts.  
;;
;; **** NOTE: This procedure does NOT draw anything, it 
;;            only returns a new figure. 
;;
;; Using the figure sqr from Exercise 5:
;; To move it up 50 units: (shift-figure sqr 0 50)
;; To move it right 100 units: (shift-figure sqr 100 0)
;;
;; Complete the function shift-figure:

 (define shift-figure
   (lambda (figure dx dy)
     (cond ((empty-figure? figure) null)
           (else (cons (make-segment (make-point (+ dx (x-coord (start (first-segment figure))))
                                                 (+ dy (y-coord (start (first-segment figure)))))
                                     (make-point (+ dx (x-coord (end (first-segment figure))))
                                                 (+ dy (y-coord (end (first-segment figure)))))
                                     (color-of (first-segment figure)))
                       (shift-figure (rest-segments figure) dx dy))))))
 
 
 (define up-sqr (shift-figure sqr 0 50))
 (define right-sqr (shift-figure sqr 100 0))
 (define move-sqr (shift-figure sqr -200 -75))

;; DRAW SQUARE AND THREE SHIFTED SQUARES
 (start-graphics)
 (draw-figure sqr)
 (draw-figure up-sqr)
 (draw-figure right-sqr)
 (draw-figure move-sqr)
 


;; Exercise 7
;; animate-figure takes a figure, a dx, a dy, a number, and a delay and
;; will create an animation that draws the figure, delays for the given
;; time period, erases the figure, shifts the figure, and repeats this
;; process n times.  To delay, use the Racket function "(sleep seconds)".
;; For example (sleep 0.1) will do nothing for a tenth of a second.
;; Complete the animate-figure function below:

 (define animate-figure
   (lambda (figure dx dy n delay)
     (cond ((<= n 0) null)
           (else (draw-figure figure)
                 (sleep delay)
                 (erase-figure figure)
                 (animate-figure (shift-figure figure dx dy)
                                 dx dy (sub1 n) delay)))))

;; ANIMATE SQUARE MOVING TO UPPER RIGHT
 (start-graphics)
 (animate-figure sqr 10 10 15 0.1)


;; Exercise 8
;;
;; Exercise your left brain!  Design a figure using any of the functions
;; we have used, or create your own custom figure.  Then, animate your
;; figure in some interesting way.  (I don't expect anything beautiful!)

(define poly 
  (make-polygon
   (list
    (make-point 0 0)
    (make-point -50 50)
    (make-point 0 15)
    (make-point 50 50))
   green))

(define poly2 (shift-figure poly -100 100))
(define poly3 (shift-figure poly 100 100))
(define poly4 (shift-figure poly 0 200))
(define poly5 (shift-figure poly -200 200))
(define poly6 (shift-figure poly 200 200))

(define geese (combine-figures poly poly2 poly3 poly4 poly5 poly6))

(start-graphics)
(animate-figure geese 25 -25 10 0.1)
(animate-figure geese -25 -25 10 0.1)
(animate-figure geese 0 -25 10 0.1)


;; Exercise 9
;;
;; WARNING: It's easy to mess up your code on this question
;; if you aren't careful. It may be a good idea to save this
;; now and then make a copy of your code before continuing.
;;
;; If you have properly built all of your functions using the
;; abstractions of segments and points, you can now add color
;; to all of your drawings.  Modify your segment abstraction
;; so that make-segment now takes an OPTIONAL color parameter.
;; If the color is not specified, or if the color does not pass
;; the rgb? test, set the color of the segment to black.  Otherwise
;; set the color of the segment to the color parameter.  
 

;; Also, add an accessor function called color-of which extracts
;; the color from a segment

;; IMPORTANT NOTE: You should comment out the old functions you are
;; replacing, keeping only these newer ones (which you should write
;; above, in place of the functions you are replacing).  If all goes
;; well, any figures you have defined above will work with your new 
;; definitions.

;; Once you have done this, you must modify the draw-segment function
;; so that it draws segments in the color you have specified.

;; IMPORTANT: After you have made these changes to the segment
;; abstraction, all of your previous code should still work,
;; with the following limitations which you should address:
;; a. make-polyline and make-polygon will only draw black lines.  Add
;;    an optional argument to each function to allow for colored lines.
;; b. If you shift a non-black figure, shift-figure should leave the
;;    color the same; currently, it makes it black.

 
 
 
;; Exercise 10
;;
;; Test your use of the abstraction: Assuming you've done everything
;; properly and used the abstraction methods, then you should be able
;; to change the underlying implementation without any need to alter
;; the remainder of your code.
;;
;; a. Change the implementation of points (make-point, x-coord, and
;; y-coord) so that instead of being a list of the x and y
;; coordinates, it is now a list of the symbol 'point, followed by a
;; cons pair of the x and the y coordinates.  For example, it would be
;; something like this: (point (100 . 0))
;;
;; b. Change the implementation of segments (make-segment, start, end,
;; and color-of) so that instead of being a list of start point, end
;; point, and color, it is now a list of the symbol 'segment, a cons
;; pair containing the start point and the end point, followed by
;; color.  For example, it would be something like this:
;; (segment ((point (100 . 0)) . (point (0 . 100))) red)
;;
;; c. Change the implementation of figures so that instead of being a
;; list of segments, it is now a list of the symbol 'figure followed
;; by each of the segments.  For example, it would something like
;; this:

;; (figure (segment ((point (100 . 0)) . (point (0 . 100))) red)
;;         (segment ((point (0 . 100)) . (point (0 . 200))) blue)
;;         (segment ((point (0 . 200)) . (point (100 . 0))) green))
;;
;; d. Be sure everything still works! 
