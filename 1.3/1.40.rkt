#lang sicp
; Exercise 1.40
; Define a procedure cubic that can be used together with the newtons-method
; procedure in expressions of the form
; (newtons-method (cubic a b c) 1)
; to approximate zeros of the cubic x^3 + ax^2 + bx + c.
; -----
;
; Recall Newton's method, fixed point and newton-transform from chapter notes
;
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

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

; Define cubic now
(define (cubic a b c)
  (define (square x) (* x x))
  (define (cube x) (* (square x) x))
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 0 0 -8) 1.0)