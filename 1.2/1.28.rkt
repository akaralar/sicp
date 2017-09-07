#lang sicp
; Exercise 1.28
; One variant of the Fermat test that cannot be fooled is called the
; Miller-Rabin test. This starts from an alternate form of Fermat’s Little
; Theorem, which states that if n is a prime number and a is any positive
; integer less than n, then a raised to the (n-1)st power is congruent to 1
; modulo n. To test the primality of a number n by the Miller-Rabin test, we
; pick a random number a < n and raise a to the (n-1)st power modulo n using
; the expmod procedure. However, whenever we perform the squaring step in
; expmod, we check to see if we have discovered a “nontrivial square root of 1
; modulo n,” that is, a number not equal to 1 or n−1 whose square is equal to 1
; modulo n. It is possible to prove that if such a nontrivial square root of 1
; exists, then n is not prime. It is also possible to prove that if n is an odd
; number that is not prime, then, for at least half the numbers a<n, computing 
; a^(n−1) in this way will reveal a nontrivial square root of 1 modulo n. (This
; is why the Miller-Rabin test cannot be fooled.) Modify the expmod procedure
; to signal if it discovers a nontrivial square root of 1, and use this to
; implement the Miller-Rabin test with a procedure analogous to fermat-test.
; Check your procedure by testing various known primes and non-primes. Hint:
; One convenient way to make expmod signal is to have it return 0.

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

(define (carmichael-number step)
  (cond ((= step 0) 561)
        ((= step 1) 1105)
        ((= step 2) 1729)
        ((= step 3) 2465)
        ((= step 4) 2821)
        ((= step 5) 6601)
        (else 0)))

(define (prime-number step)
  (cond ((= step 0) 3)
        ((= step 1) 7)
        ((= step 2) 11)
        ((= step 3) 1009)
        ((= step 4) 10037)
        ((= step 5) 100043)
        (else 0)))

(define (test-carmichael step)
  (cond ((= (carmichael-number step) 0)
         (newline)
         (display "Finished"))
        (else
         (newline)
         (display (carmichael-number step))
         (display " ")
         (display (miller-rabin-test (carmichael-number step)))
         (test-carmichael (+ step 1)))))

(define (test-prime step)
  (cond ((= (prime-number step) 0)
         (newline)
         (display "Finished"))
        (else
         (newline)
         (display (prime-number step))
         (display " ")
         (display (miller-rabin-test (prime-number step)))
         (test-prime (+ step 1)))))

(test-carmichael 0)
(test-prime 0)


