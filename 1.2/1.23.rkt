#lang sicp
; Exercise 1.23
; (define (smallest-divisor n)
;   (find-divisor n 2))
;
; (define (find-divisor n test-divisor)
;   (cond ((> (square test-divisor) n)
;          n)
;         ((divides? test-divisor n)
;          test-divisor)
;         (else (find-divisor
;                n
;                (+ test-divisor 1)))))
; 
; (define (divides? a b)
;   (= (remainder b a) 0))

; The smallest-divisor procedure shown above does lots of needless testing:
; After it checks to see if the number is divisible by 2 there is no point in
; checking to see if it is divisible by any larger even numbers. This suggests
; that the values used for test-divisor should not be 2, 3, 4, 5, 6, ..., but
; rather 2, 3, 5, 7, 9, .... To implement this change, define a procedure "next"
; that returns 3 if its input is equal to 2 and otherwise returns its input
; plus 2. Modify the smallest-divisor procedure to use (next test-divisor)
; instead of (+ test-divisor 1). With timed-prime-test incorporating this
; modified version of smallest-divisor, run the test for each of the 12 primes
; found in Exercise 1.22. Since this modification halves the number of test
; steps, you should expect it to run about twice as fast. Is this expectation
; confirmed? If not, what is the observed ratio of the speeds of the two
; algorithms, and how do you explain the fact that it is different from 2?”

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (next test-divisor)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next divisor)
  (if (= divisor 2)
      3
      (+ divisor 2)))

; Incorporating solution from Exercise 1.22

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

; Times for previous exercise:
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
; Times for this run
; #.   Prime *** Time
; 1.    1009 *** 3
;       1013 *** 4
;       1019 *** 3
; 2.   10007 *** 7
;      10009 *** 8
;      10037 *** 7
; 3.  100003 *** 20
;     100019 *** 19
;     100043 *** 19
; 4. 1000003 *** 56
;    1000033 *** 57
;    1000037 *** 56
; 
; Theoretically the times should be halved since the new algorithm is twice as
; fast. Dividing averages for each run with averages from Exercise 1.22 we find
;   run 1 = 1.2
;   run 2 = 1.41
;   run 3 = 1.57
;   run 4 = 1.72