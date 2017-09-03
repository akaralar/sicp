#lang sicp
; Exercise 1.19
; There is a clever algorithm for computing the Fibonacci numbers in a
; logarithmic number of steps. Recall the transformation of the state variables
; a and b in the fib-iter process of 1.2.2: 
; a ← a + b and b ← a
;
; Call this transformation T, and observe that applying T over and over again 
; n times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n). In
; other words, the Fibonacci numbers are produced by applying T^n, the nth
; power of the transformation T, starting with the pair (1, 0). Now consider T
; to be the special case of p = 0 and q = 1 in a family of transformations 
; Tpq, where Tpq transforms the pair (a,b) according to
; a ← bq + aq + ap and b ← bp + aq.

; Show that if we apply such a transformation Tpq twice, the effect is the same
; as using a single transformation Tp′q′ of the same form, and compute  p′ and 
; q′ in terms of p and q. This gives us an explicit way to square these
; transformations, and thus we can compute T^n using successive squaring, as in
; the fast-expt procedure. Put this all together to complete the following
; procedure, which runs in a logarithmic number of steps:”

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (square x) (* x x))

(define (fib-iter a b p q count)
  (cond ((= count 0)
         b)
        ((even? count)
         (fib-iter
          a
          b
          (+ (square p) (square q))
          (+ (* 2 p q) (square q))
          (/ count 2)))
        (else
         (fib-iter
          (+ (* b q) (* a q) (* a p))
          (+ (* b p) (* a q))
          p
          q
          (- count 1)))))

; We have (a, b), and transformation T,
; after T1: (ap + aq + bq, aq + bp)
; after T2: (ap^2 + 2apq + 2aq^2 + 2bpq + bq^2,
;            2apq + aq^2 + bp^2 + bq^2)
; and this should be equal to (ap′ + aq′ + bq′, aq′ + bp′) both in form and in
; value. Solving for p′ and q′, we find
; p′ = p^2 + q^2
; q′ = 2pq + q^2
