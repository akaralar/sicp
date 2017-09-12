#lang sicp
; Exercise 1.29
; Simpson’s Rule is a more accurate method of numerical integration than the
; method illustrated above. Using Simpson’s Rule, the integral of a function f
; between a and b is approximated as
;
; h/3*(y0+(4*y1)+(2*y2)+(4*y3)+(2*y4)+⋯+(2*y(n-2))+(4*y(n−1))+yn),
; where h = (b−a)/n, for some even integer n, and yk = f(a+kh).
; (Increasing n increases the accuracy of the approximation.)
;
; Define a procedure that takes as arguments f, a, b, and n and returns the
; value of the integral, computed using Simpson’s Rule. Use your procedure to
; integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare the
; results to those of the integral procedure shown above.
; -----
;
; From the book, for summation we have
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Defining Simpson's Rule
(define (simpson-integral f a b n)
  (define (h) (/ (- b a) n))
  (define (y k) (f (+ a (* k (h)))))
  (define (coeff k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k) (* (coeff k) (y k)))
  (define (inc k) (+ k 1))
  (* (/ (h) 3) (sum term 0 inc n))) 

(define (cube x) (* x x x))
(simpson-integral cube 0.0 1.0 100)  ; 0.24999999999999992
(simpson-integral cube 0.0 1.0 1000) ; 0.2500000000000003

; Both results are more accurate than previous method