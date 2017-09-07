#lang sicp
; Exercise 1.24
; Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime?
; (the Fermat method), and test each of the 12 primes you found in that
; exercise. Since the Fermat test has Θ(log n) growth, how would you expect the
; time to test primes near 1,000,000 to compare with the time needed to test
; primes near 1000? Do your data bear this out? Can you explain any discrepancy
; you find?”

(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

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

; We test 10 times with fast prime
; #.   Prime *** Time
; 1.    1009 *** 18
;       1013 *** 19
;       1019 *** 19
; 2.   10007 *** 24
;      10009 *** 22
;      10037 *** 25
; 3.  100003 *** 28
;     100019 *** 27
;     100043 *** 30
; 4. 1000003 *** 30
;    1000033 *** 31
;    1000037 *** 32
;
; Ratios of averages for each run should be O(log n2) / O(log n21)
;   run 2 / run 1 = log(10000)   / log(1000)   = 1.33 
;   run 3 / run 2 = log(100000)  / log(10000)  = 1.24
;   run 4 / run 3 = log(1000000) / log(100000) = 1.2
;
; Our solution calculates
;   run 2 / run 1 = 1.27
;   run 3 / run 2 = 1.20
;   run 4 / run 3 = 1.09
; which can be considered in line with O(log n) assumption.
;
; For the question of ratio of 4th run(1,000,000) to ratio of 1st run (1,000)
; we expect the 4th run to take twice as long since log(1000000)/log(1000) = 2
; but we find that it's 1.66 instead.