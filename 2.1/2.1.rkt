#lang sicp
; Exercise 2.1
; Define a better version of make-rat that handles both positive and negative
; arguments. Make-rat should normalize the sign so that if the rational number
; is positive, both the numerator and denominator are positive, and if the
; rational number is negative, only the numerator is negative.
; -----
; Recall make-rat with gcd from notes:
(define (gcd a b)
  (if (= b 0)
  a
  (gcd b (remainder a b))))

(define (make-rat-gcd n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

; Now we need to make a better rat that handles negatives
(define (better-make-rat n d)
  (define (negative? x) (< x 0))
  (define (negative-rat? n d)
    (and (or (negative? n)
             (negative? d))
         (not (and (negative? n)
                   (negative? d)))))
  (let ((g (gcd (abs n) (abs d))))
    (cons (/ (abs n) g)
          (if (negative-rat? n d)
              (/ (- (abs d)) g)
              (/ (abs d) g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat-gcd x y)
  (better-make-rat (+ (* (numer x) (denom y))
                   (* (numer y) (denom x)))
                (* (denom x) (denom y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (better-make-rat -5 -6))
(print-rat (better-make-rat -5 6))
(print-rat (better-make-rat 5 -6))
