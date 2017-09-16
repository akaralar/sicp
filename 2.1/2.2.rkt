#lang sicp
; Exercise 2.2
; Consider the problem of representing line segments in a plane. Each segment
; is represented as a pair of points: a starting point and an ending point.
; Define a constructor make-segment and selectors start-segment and end-segment
; that define the representation of segments in terms of points. Furthermore, a
; point can be represented as a pair of numbers: the x coordinate and the y
; coordinate. Accordingly, specify a constructor make-point and selectors
; x-point and y-point that define this representation. Finally, using your
; selectors and constructors, define a procedure midpoint-segment that takes a
; line segment as argument and returns its midpoint (the point whose
; coordinates are the average of the coordinates of the endpoints). To try your
; procedures, you’ll need a way to print points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment segment)
  (let ((start-x (x-point (start-segment segment)))
        (start-y (y-point (start-segment segment)))
        (end-x (x-point (end-segment segment)))
        (end-y (y-point (end-segment segment))))
    (make-point (/ (+ start-x end-x) 2.0)
                (/ (+ start-y end-y) 2.0))))

(define segment (make-segment (make-point 2 3) (make-point 9 20)))
(print-point (midpoint-segment segment))