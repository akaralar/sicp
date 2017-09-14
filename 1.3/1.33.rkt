#lang sicp
; Exercise 1.33
; You can obtain an even more general version of accumulate (Exercise 1.32) by
; introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting filtered-accumulate abstraction takes the
; same arguments as accumulate, together with an additional predicate of one
; argument that specifies the filter. Write filtered-accumulate as a procedure.
; Show how to express the following using filtered-accumulate:”
;   1. the sum of the squares of the prime numbers in the interval a to b
;      (assuming that you have a prime? predicate already written)
;   2. the product of all the positive integers less than n that are relatively
;      prime to n (i.e., all positive integers i<n such that GCD(i,n)=1).
; -----
;
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (filtered-accumulate
                 combiner
                 null-value
                 term
                 (next a)
                 next
                 b
                 filter))))


; 1.
; Recall Miller Rabin Test for primes from Exercise 1.28
(define (square x) (* x x))

(define (expmod base exp m)
  (define (nontrivial-sqrt? x)
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= (remainder (square x) m) 1))
        0
        x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (nontrivial-sqrt? (expmod base (/ exp 2) m)))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(#%require (only racket/base random))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (define (test times)
    (if (= times 0)
        true
        (and (try-it (+ 1 (random (- n 1)))) (test (- times 1)))))
  (test 10))

; We can define prime? as this
(define (prime? x) (miller-rabin-test x))

; Defining sum of squares of prime numbers in interval (a, b) 
(define (sum-square-of-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))
(sum-square-of-prime 2 10)
; 2.
; Recall GCD from 1.2.5
(define (gcd a b)
  (if (= b 0)
  a
  (gcd b (remainder a b))))

; Defining product of relative primes of n in 2 < x < n 
(define (relatively-prime-product n)
  (define (relatively-prime? x) (= (gcd n x) 1))
  (filtered-accumulate * 1 identity 2 inc n relatively-prime?))

(relatively-prime-product 10) 