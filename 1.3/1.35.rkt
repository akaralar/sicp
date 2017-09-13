#lang sicp
; Exercise 1.35
; Show that the golden ratio φ (1.2.2) is a fixed point of the transformation 
; x ↦ 1 + 1/x
; and use this fact to compute φ by means of the fixed-point procedure.
; -----
; From 1.2.2, we see that 
; φ = (1 + 5^(1/2)) / 2
; Being fixed point of transformation implies
; φ = 1 + 1/φ
; Substituting equation from 1.2.2 for φ
; (1 + 5^(1/2)) / 2 = 1 + 1/((1 + 5^(1/2)) / 2)
; (1 + 5^(1/2)) / 2 = (3 + 5^(1/2)) / (1 + 5^(1/2))
;
; Multiplying both sides with 2φ = 1 + 5^(1/2)
; (1 + 5^(1/2))^2 / 2 = 3 + 5^(1/2)
; (1 + 2*5^(1/2) + 5) / 2 = 3 + 5^(1/2)
; 3 + 5^(1/2) = 3 + 5^(1/2)
; Hence, φ is a fixed point for x ↦ 1 + 1/x

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

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)