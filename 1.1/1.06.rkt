#lang sicp
; Alyssa P. Hacker doesn’t see why if needs to be provided
; as a special form. “Why can’t I just define it as an
; ordinary procedure in terms of cond?” she asks. Alyssa’s
; friend Eva Lu Ator claims this can indeed be done, and she
; defines a new version of if:”

(define (new-if predicate
         then-clause
         else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

; Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

; Infinite loop, but why?