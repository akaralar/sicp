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

(define (find-roots a b result completion)
  (cond ((= (remainder result x) 0)
         (find-roots (+ a 1) b (/ result x) completion))
        ((= (remainder result y) 0)
         (find-roots a (+ b 1) (/ result y) completion))
        (else (completion a b))))
(define (car i)
  (find-roots 0 0 i (lambda (a b) a)))
(define (cdr i)
  (find-roots 0 0 i (lambda (a b) b)))

(define a 3)
(define b 4)
(cons a b)
(= (car (cons a b)) a)
(= (cdr (cons a b)) b)