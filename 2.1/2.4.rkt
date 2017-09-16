#lang sicp
; Exercise 2.4
; Here is an alternative procedural representation of pairs. For this
; representation, verify that (car (cons x y)) yields x for any objects x and y.
; -----
(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))

; The corresponding definition for cdr returns q in the lambda
(define (cdr z)
  (z (lambda (p q) q)))

(define x 3)
(define y 5)
(= (car (cons x y)) x)
(= (cdr (cons x y)) y)