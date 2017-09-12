#lang sicp
; Exercise 1.36
; Modify fixed-point so that it prints the sequence of approximations it
; generates, using the newline and display primitives shown in Exercise 1.22.
; Then find a solution to x^x = 1000 by finding a fixed point of
; x ↦ log⁡(1000)/log(x). (Use Scheme’s primitive log procedure, which computes
; natural logarithms.)
; Compare the number of steps this takes with and without average damping.
; (Note that you cannot start fixed-point with a guess of 1, as this would
; cause division by log(1) = 0.)
; -----
; Modify fixed point to print values
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (display "guess: ")
    (display v1)
    (display  " next: ")
    (display v2)
    (newline)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
    (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

; We start guesses at 2.0
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)
; Define average to apply average damping
(define (average x y) (/ (+ x y) 2))
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)

; Average damping converges much sooner than without damping.