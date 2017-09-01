#lang sicp
; Define a procedure that takes three numbers as arguments
; and returns the sum of the squares of the two larger numbers.

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (>= x y) (not (< x y)))
(define (sum-of-highs x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= z x) (>= y x)) (sum-of-squares y z))
        ((and (>= z y) (>= x y)) (sum-of-squares x z))))
