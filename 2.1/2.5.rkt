#lang sicp
; Exercise 2.5
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if we represent the pair a and b as the integer
; that is the product 2^a*3^b. Give the corresponding definitions of the
; procedures cons, car, and cdr
; -----
(define x 2)
(define y 3)

(define (cons a b) (* (expt x a) (expt y b)))

(define a 3)
(define b 4)
(cons a b)
(define (car i)
  (define (iter a b result)
    (cond ((= (remainder result x) 0)
           (iter (+ a 1) b (/ result x)))
          ((= (remainder result y) 0)
           (iter a (+ b 1) (/ result y)))
          (else a)))
  (iter 0 0 i))
(= (car (cons a b)) a)

(define (cdr i)
  (define (iter a b result)
    (cond ((= (remainder result x) 0)
           (iter (+ a 1) b (/ result x)))
          ((= (remainder result y) 0)
           (iter a (+ b 1) (/ result y)))
          (else b)))
  (iter 0 0 i))
(= (cdr (cons a b)) b)

           
           
          