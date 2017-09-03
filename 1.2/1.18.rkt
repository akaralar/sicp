#lang sicp
; Exercise 1.18
; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that
; generates an iterative process for multiplying two integers in terms of
; adding, doubling, and halving and uses a logarithmic number of steps

(define (double x) (+ x x))
(define (halve x) (/ x 2.0))

(define (fast-*-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-*-iter (double a) (halve b) sum))
        (else (fast-*-iter a (- b 1) (+ sum a)))))

(define (fast-* a b)
  (fast-*-iter a b 0))

(fast-* 3 5)
(fast-* 9 11)
(fast-* 3 1)
(fast-* 52 43)