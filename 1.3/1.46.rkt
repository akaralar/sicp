#lang sicp
; Exercise 1.46
; Several of the numerical methods described in this chapter are instances of
; an extremely general computational strategy known as iterative improvement.
; Iterative improvement says that, to compute something, we start with an
; initial guess for the answer, test if the guess is good enough, and otherwise
; improve the guess and continue the process using the improved guess as the
; new guess. Write a procedure iterative-improve that takes two procedures as
; arguments: a method for telling whether a guess is good enough and a method
; for improving a guess. Iterative-improve should return as its value a
; procedure that takes a guess as argument and keeps improving the guess until
; it is good enough. Rewrite the sqrt procedure of 1.1.7 and the fixed-point
; procedure of 1.3.3 in terms of iterative-improve.
; -----
;
(define (iterative-improve good-enough? improve)
  (define (improve-guess guess)
    (if (good-enough? guess)
        guess
        (improve-guess (improve guess))))
  (lambda (guess) (improve-guess guess)))

((iterative-improve (lambda (x) (> x 10))
                   (lambda (x) (+ x 1))) 1)

; Recall sqrt from 1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2.0))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (square x) (* x x))


; Define sqrt in terms of iterative-improve and apply with x
(define (sqrt-improve x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                     (lambda (guess) (average guess (/ x guess))))
   x))

(sqrt-improve 256) ; 16.00000352670594

; Recall fixed-point from 1.3.3
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

; Define fixed-point in terms of iterative-improve and apply first-guess as the
; parameter
(define (fixed-point-improve f first-guess)
  ((iterative-improve (lambda (x) (< (abs (- x (f x))) tolerance))
                      f)
   first-guess))

(fixed-point-improve cos 1.0)
(fixed-point-improve (lambda (y) (+ (sin y) (cos y)))
                     1.0)
(define (sqrt-fixed x)
  (fixed-point-improve (lambda (y) (average y (/ x y)))
                       1.0))
(sqrt-fixed 81)
