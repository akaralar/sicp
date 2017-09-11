#lang sicp
; Exercise 1.27
; Demonstrate that the Carmichael numbers listed in footnote 47 really do fool
; the Fermat test. That is, write a procedure that takes an integer n and tests
; whether a^n is congruent to a modulo n for every a < n, and try your
; procedure on the given Carmichael numbers.”
; ------
; Carmichael numbers given in footnote 47: 561, 1105, 1729, 2465, 2821, 6601
;
; Recall expmod method from previous exercises to find a modulo to an
; exponentiation

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


(define (all-congruent-modulo? n)
  (define (congruent-modulo? a)
    (cond ((not (< a n)) false)
          ((= a 0) true)
          (else (and
                 (= (expmod a n n) a)
                 (congruent-modulo? (- a 1))))))
  (congruent-modulo? (- n 1)))
    
(define (carmichael-number step)
  (cond ((= step 0) 561)
        ((= step 1) 1105)
        ((= step 2) 1729)
        ((= step 3) 2465)
        ((= step 4) 2821)
        ((= step 5) 6601)
        (else 0)))

(define (test-carmichael step)
  (cond ((= (carmichael-number step) 0) true)
        (else (and
               (all-congruent-modulo? (carmichael-number step))
               (test-carmichael (+ step 1))))))

(test-carmichael 0)
