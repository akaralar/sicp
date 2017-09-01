#lang sicp
; Exercise 1.8.
; Newton’s method for cube roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is given by
; the value
;
; (x/y^2 + 2y) / 3
;
; Use this formula to implement a cube-root procedure analogous to the
; square-root procedure.

(define (square x) (* x x))
(define (improve guess x)
  (/
   (+
    (/ x (square guess))
    (* guess 2))
   3))
   
(define (good-enough? guess improved-guess)
  (> 0.00000001 (/ (abs (- improved-guess guess)) guess)))

(define (cube-root-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cube-root-iter (improve guess x) x)))

(define (cbrt x) (cube-root-iter 1.0 x))

(cbrt 64)
(cbrt 0.001)

