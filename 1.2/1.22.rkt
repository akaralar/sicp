#lang sicp
; Exercise 1.22
; Most Lisp implementations include a primitive called runtime that returns an
; integer that specifies the amount of time the system has been running
; (measured, for example, in microseconds). The following timed-prime-test
; procedure, when called with an integer n, prints n and checks to see if n is
; prime. If n is prime, the procedure prints three asterisks followed by the
; amount of time used in performing the test.
;
; (define (timed-prime-test n)
;   (newline)
;   (display n)
;   (start-prime-test n (runtime)))
;
; (define (start-prime-test n start-time)
;   (if (prime? n)
;       (report-prime (- (runtime)
;                        start-time))))
;
; (define (report-prime elapsed-time)
;   (display " *** ")
;   (display elapsed-time))
;
; Using this procedure, write a procedure search-for-primes that checks the
; primality of consecutive odd integers in a specified range. Use your
; procedure to find the three smallest primes larger than 1000; larger than
; 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to
; test each prime. Since the testing algorithm has order of growth of
; Θ(n^1/2), you should expect that testing for primes around 10,000 should
; take about 10^1/2 times as long as testing for primes around 1000. Do your
; timing data bear this out? How well do the data for 100,000 and 1,000,000
; support the Θ(n^1/2) prediction? Is your result compatible with the notion
; that programs on your machine run in time proportional to the number of steps
; required for the computation?”

(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

; Modifying start-prime-test to return boolean
(define (start-prime-test n start-time)
  (cond ((prime? n)
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

; #.   Prime *** Time
; 1.    1009 *** 4
;       1013 *** 4
;       1019 *** 4
; 2.   10007 *** 11
;      10009 *** 10
;      10037 *** 10
; 3.  100003 *** 31
;     100019 *** 30
;     100043 *** 30
; 4. 1000003 *** 94
;    1000033 *** 94
;    1000037 *** 102
;
; Theoretically the ratios of consecutive runs should equal 10^1/2 = 3.16.
; Taking averages of runs and dividing them we find the order of growths to be
;   run 2 / run 1 = 2.58
;   run 3 / run 2 = 2.94
;   run 4 / run 3 = 3.18
; Probably because more runs to find primes were made in the later runs,
; they are less prone to statistical errors and closer to the theoretical value