#lang sicp
; 2. Building Absractions with Data
(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

; 2.1 Introduction to data abstraction

(define x (cons 1 2))
(car x)
(cdr x)

(define y (cons 1 2))
(define z (cons 3 4))
(define a (cons y z))
(car (car a))

(car (cdr a))

(define (make-rat n d ) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat
 (add-rat one-half one-third))
(print-rat
 (mul-rat one-half one-third))
(print-rat
 (add-rat one-third one-third))

; Recall gcd from 1.2.5
(define (gcd a b)
  (if (= b 0)
  a
  (gcd b (remainder a b))))

(define (make-rat-gcd n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

(define (add-rat-gcd x y)
  (make-rat-gcd (+ (* (numer x) (denom y))
                   (* (numer y) (denom x)))
                (* (denom x) (denom y))))

(print-rat
 (add-rat-gcd one-third one-third))
