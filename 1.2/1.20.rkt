#lang sicp
; Exercise 1.20
; The process that a procedure generates is of course dependent on the rules
; used by the interpreter. As an example, consider the iterative greatest
; common divisor procedure:
;
; (define (gcd a b)
;  (if (= b 0)
;      a
;      (gcd b (remainder a b))))
;
; Suppose we were to interpret this procedure using normal-order evaluation, as
; discussed in 1.1.5. (The normal-order-evaluation rule for if is described in
; Exercise 1.5.) Using the substitution method (for normal order), illustrate
; the process generated in evaluating (gcd 206 40) and indicate the remainder
; operations that are actually performed. How many remainder operations are
; actually performed in the normal-order evaluation of (gcd 206 40)? In the
; applicative-order evaluation?

; a) Normal-order evaluation
;
;    gcd(206, 40) converges to a solution after 5 iterations
;
;    1. gcd(206, 40)
;    2. gcd(40, 6)
;    3. gcd(6, 4)
;    4. gcd(4, 2)
;    5. gcd(2, 0)
;
;    Substituting gcd at each step, we have after iteration 5:
(define (gcd a b)
 (if (= b 0)
     a
     (if (= (remainder a b) 0)
         b
         (if (= (remainder b (remainder a b)) 0)
             (remainder a b)
             (if (= (remainder (remainder a b) (remainder b (remainder a b))) 0)
                 (remainder b (remainder a b))
                 (if (= (remainder (remainder b (remainder a b)) (remainder (remainder a b) (remainder b (remainder a b)))) 0)
                     (remainder (remainder a b) (remainder b (remainder a b)))
                     -5)))))) ; Test value to see if the substition works as
                              ; expected, since this line shouldn't be
                              ; evaluated when number of steps < 5

;    We would evaluate all "if"s, alternative part of "if" in each "if" except
;    the last and we will evaluate the consequent part in the last if. In total
;    this will make 14 operations in "if" predicates and 4 operations in the
;    consequent of last if, making 18 operations when evaluated with
;    normal-order evaluation.
;
; b) Applicative-order evaluation
;
;    In applicative order evaluation, the remainder operation is evaluated once
;    at each step when the condition is false, and it's not evaluated when the
;    condition is true. The condition is false except the last time, so
;    5 - 1 = 4 remainder operations will be evaluated.

         
