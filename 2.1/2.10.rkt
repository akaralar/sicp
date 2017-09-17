#lang sicp
; Exercise 2.10
; Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and
; comments that it is not clear what it means to divide by an interval that
; spans zero. Modify Alyssa’s code to check for this condition and to signal an
; error if it occurs.
; -----
; Recall divide
(#%require "2.7.rkt")
(#%require "2.8.rkt")

(define (spans x interval)
  (and (> x (- (lower-bound interval) 1))
       (< x (+ (upper-bound interval) 1))))
         
(define (div-interval x y)
  (cond ((spans 0 y)
         (display "Error: y spans zero")
         (newline))
        (else (mul-interval x
                            (make-interval (/ 1.0 (upper-bound y))
                                           (/ 1.0 (lower-bound y)))))))


(define test-interval-1 (make-interval 3 9))
(define test-interval-2 (make-interval -1 10))
(div-interval test-interval-1 test-interval-2)