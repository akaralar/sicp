#lang sicp
; Exercise 1.39
; A continued fraction representation of the tangent function was published in
; 1770 by the German mathematician J.H. Lambert:
;
;                 x
; tan x =  ---------------------
;                       x^2
;           1 -  ---------------
;                         x^2
;                 3 - ----------
;                       5 - ....
;
; where x is in radians.
;
; Define a procedure (tan-cf x k) that computes an approximation to the tangent
; function based on Lambert’s formula. k specifies the number of terms to
; compute, as in Exercise 1.37.
; -----

(define (tan-cf x k)
  (define (iter step)
    (let ((n (if (= step 1) x (* x x)))
          (d (- (* step 2.0) 1.0)))
      (if (> step k)
          0
          (/ n (- d (iter (+ step 1)))))))
  (iter 1))

(tan-cf 1.0 100)