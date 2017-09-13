#lang sicp
; Exercise 1.39
; A continued fraction representation of the tangent function was published in
; 1770 by the German mathematician J.H. Lambert:
; tan x = x / (1 - (x^2 / (3 - (x^2 / (5 - ..., where x is in radians.
; Define a procedure (tan-cf x k) that computes an approximation to the tangent
; function based on Lambert’s formula. k specifies the number of terms to
; compute, as in Exercise 1.37.
; -----

(define (tan-cf x k)
  (define (n k) (if (= k 1) x (* x x)))
  (define (d k) (- (* k 2.0) 1.0))
  (define (iter step)
    (if (= step k)
        (/ (n step) (d step))
        (/ (n step) (- (d step) (iter (+ step 1))))))
  (iter 1))

(tan-cf 1.0 10)