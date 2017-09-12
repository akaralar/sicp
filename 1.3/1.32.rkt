#lang sicp
; Exercise 1.32
; 1) Show that sum and product (Exercise 1.31) are both special cases of a still
;    more general notion called accumulate that combines a collection of terms,
;    using some general accumulation function:
;
;    (accumulate 
;     combiner null-value term a next b)
;
;    Accumulate takes as arguments the same term and range specifications as sum
;    and product, together with a combiner procedure (of two arguments) that
;    specifies how the current term is to be combined with the accumulation of
;    the preceding terms and a null-value that specifies what base value to use
;    when the terms run out. Write accumulate and show how sum and product can
;    both be defined as simple calls to accumulate.
;
; 2) If your accumulate procedure generates a recursive process, write one that
;    generates an iterative process. If it generates an iterative process,
;    write one that generates a recursive process.
; -----
;
; Solution 1)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

; Writing sum in terms of accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b))

; Writing product in terms of accumulate
(define (product term a next b)
  (accumulate * 1 term a next b))

; Define square, inc and identity to test
(define (square x) (* x x))
(define (inc x) (+ 1 x))
(define (identity x) x)

(sum square 1 inc 5)
(product identity 1 inc 5)

; Solution 2)
; Our accumulate procedure generates a recursive process, so we need to write an
; iterative process

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; Writing sum and product with iterative accumulate
(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))
(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))
        
(sum-iter square 1 inc 5)
(product-iter identity 1 inc 5)
