#lang sicp
; Exercise 1.45
; We saw in 1.3.3 that attempting to compute square roots by naively finding a
; fixed point of y ↦ x/y does not converge, and that this can be fixed by
; average damping. The same method works for finding cube roots as fixed points
; of the average-damped y ↦ x/y^2. Unfortunately, the process does not work for
; fourth roots—a single average damp is not enough to make a fixed-point search
; for y ↦ x/y^3 converge. On the other hand, if we average damp twice (i.e.,
; use the average damp of the average damp of y ↦ x/y^3) the fixed-point search
; does converge. Do some experiments to determine how many average damps are
; required to compute nth roots as a fixed-point search based upon repeated
; average damping of y ↦ x/y^(n-1). Use this to implement a simple procedure
; for computing nth roots using fixed-point, average-damp, and the repeated
; procedure of Exercise 1.43. Assume that any arithmetic operations you need
; are available as primitives.
; -----
;
; Recall average-damp
(define (average x y) (/ (+ x y) 2.0))
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

; Recall fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
    (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

; Recall compose and repeated from 1.43
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f count)
  (cond ((= count 0) (lambda (x) x))
        ((= count 1) (lambda (x) (f x)))
        (else (compose f (repeated f (- count 1))))))

; Define pow to compute x^y
(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))

; Define computing nth roots of x
(define number-of-damps 5)
(define (nth-root x n)
  (fixed-point ((repeated average-damp
                          number-of-damps) (lambda (y) (/ x (pow y (- n 1)))))
               1.0))
               

(nth-root 81 32)
;  n --- number of damps to converge
;  1 --- 0
;  2 --- 1
;  3 --- 1
;  4 --- 2
;  5 --- 2
;  6 --- 2
;  7 --- 2
;  8 --- 3
; ...
; 15 --- 3
; 16 --- 4
; ...
; 32 --- 5

; So we see that we need log(n) damps to converge