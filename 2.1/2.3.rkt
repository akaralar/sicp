#lang sicp
; Exercise 2.3
; Implement a representation for rectangles in a plane. (Hint: You may want to
; make use of Exercise 2.2.) In terms of your constructors and selectors,
; create procedures that compute the perimeter and the area of a given
; rectangle. Now implement a different representation for rectangles. Can you
; design your system with suitable abstraction barriers, so that the same
; perimeter and area procedures will work using either representation?
; -----
; Recall point from Exercise 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Defining rects
; 1. A rect is a point for origin and a size
; Define size to represent rectangle
(define (make-size width height) (cons width height))
(define (width-size size) (car size))
(define (height-size size) (cdr size))
(define (print-size p)
  (display "width: ")
  (display (width-size p))
  (display ", height: ")
  (display (height-size p)))

(define (make-rect origin size) (cons origin size))
(define (origin rect) (car rect))
(define (size rect) (cdr rect))
(define (print-rect r)
  (newline)
  (display "origin: ")
  (print-point (origin r))
  (display ", ")
  (print-size (size r))
  (newline))

; 2. A rect is two non-neigbour points
(define (make-rect-2 p1 p2) (cons p1 p2))
(define (p1-rect rect) (car rect))
(define (p2-rect rect) (cdr rect))

; We can define width and height helpers, these are all we need to compute
; perimeter and area. Implementations for these will be different for different
; representations of rects but this will ensure our implementations for
; perimeter and area will remain unchanged.
; 1. representation
;(define (width-rect rect) (width-size (size rect)))
;(define (height-rect rect) (height-size (size rect)))

; 2. representation
(define (width-rect rect)
  (let ((p1 (p1-rect rect))
        (p2 (p2-rect rect)))
    (abs (- (x-point p1) (x-point p2)))))
(define (height-rect rect)
  (let ((p1 (p1-rect rect))
        (p2 (p2-rect rect)))
    (abs (- (y-point p1) (y-point p2)))))

; Perimeter is twice the total of width and height
(define (perimeter rect)
  (* 2 (+ (width-rect rect) (height-rect rect))))

; Area is width * height
(define (area rect)
  (* (width-rect rect) (height-rect rect)))

; 1.
;(define test-rect (make-rect (make-point 3 5) (make-size 100 50)))
;(print-rect test-rect)
;(perimeter test-rect)
;(area test-rect)
; 2.
(define test-rect-2 (make-rect-2 (make-point 3 5) (make-point 53 105)))
(perimeter test-rect-2)
(area test-rect-2)

    
