#lang sicp
; Exercise 1.38
; In 1737, the Swiss mathematician Leonhard Euler published a memoir De
; Fractionibus Continuis, which included a continued fraction expansion for
; e - 2, where e is the base of the natural logarithms. In this fraction, the 
; Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ….
; Write a program that uses your cont-frac procedure from Exercise 1.37 to
; approximate e, based on Euler’s expansion.
; -----
; Recall cont-frac from 1.37
(define (cont-frac n d k)
  (define (iter x result)
    (let ((n (n x))
          (d (d x)))
    (if (= x 0)
        result
        (iter (- x 1) (/ n (+ d result))))))
  (iter k 0))

; Ni will be 1 and Di will be (floor(x/3)+1)*2 when x % 3 = 2
(define e-2 (cont-frac
             (lambda (x) 1.0)
             (lambda (x) (if (= (remainder x 3) 2)
                             (* 2 (+ 1 (quotient x 3)))
                             1))
 100))
e-2
; and e = e-2 + 2 
(+ e-2 2)