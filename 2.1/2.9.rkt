#lang sicp
; Exercise 2.9
; The width of an interval is half of the difference between its upper and
; lower bounds. The width is a measure of the uncertainty of the number
; specified by the interval. For some arithmetic operations the width of the
; result of combining two intervals is a function only of the widths of the
; argument intervals, whereas for others the width of the combination is not a
; function of the widths of the argument intervals. Show that the width of the
; sum (or difference) of two intervals is a function only of the widths of the
; intervals being added (or subtracted). Give examples to show that this is not
; true for multiplication or division.
; -----
; First define width-interval
(#%require "2.7.rkt")
(#%require "2.8.rkt")
(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval))
     2.0))

(define test-interval-1 (make-interval 13 35))
(define test-interval-2 (make-interval 2 9))
(width-interval test-interval-1)
(width-interval test-interval-2)

(width-interval (add-interval test-interval-1 test-interval-2))
(width-interval (sub-interval test-interval-1 test-interval-2))
; We see that addition and subtraction are just adding and subtracting the width

(width-interval (mul-interval test-interval-1 test-interval-2))
(width-interval (div-interval test-interval-1 test-interval-2))
; For multiplication and division, the width doesn't seem to be a function of
; widths of intervals

  

