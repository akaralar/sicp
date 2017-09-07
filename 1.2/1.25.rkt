#lang sicp
; Exercise 1.25
; Alyssa P. Hacker complains that we went to a lot of extra work in writing
; expmod. After all, she says, since we already know how to compute
; exponentials, we could have simply written

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; Is she correct? Would this procedure serve as well for our fast prime tester?
; Explain.


(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))
(define (square x) (* x x))

; If m = 0 in expmod, this version crashes but since in Fermat test n is a
; prime, most likely 0 will never be tested since it's known to not be a prime
; Hence the version Alyssa P. Hacker proposes produces correct results. However,
; if we run search-for-primes with this version of expmod, we see it takes
; considerably longer to produce results. This is because the previous version
; of the expmod took a modulo before returning, hence keeping return values of
; expmod smaller than m at each step. With Alyssa's version, fast-exp calculates
; giant numbers at intermediate steps, hence taking a lot longer at especially
; larger values.

(#%require (only racket/base random))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

; Modifying start-prime-test to return boolean
(define (start-prime-test n start-time)
  (cond ((fast-prime? n 10)
         (report-prime (- (runtime) start-time))
         true)
        (else false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end primes-to-find)
  (cond ((= primes-to-find 0) (newline) (display "Finished"))
        ((> start end) (display "Range ended"))
        ((even? start) (search-for-primes (+ 1 start) end primes-to-find))
        ((timed-prime-test start) 
         (search-for-primes (+ 2 start) end (- primes-to-find 1)))
        (else
         (search-for-primes (+ 2 start) end primes-to-find))))

(search-for-primes 1000 10000 3)       ; 1
(search-for-primes 10000 100000 3)     ; 2
(search-for-primes 100000 1000000 3)   ; 3
(search-for-primes 1000000 10000000 3) ; 4
