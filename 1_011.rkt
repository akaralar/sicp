#lang planet neil/sicp

#|“Exercise 1.11.  A function f is defined by the rule that f(n) = n if n<3 
   and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>=3. Write a procedure that 
   computes f by means of a recursive process. Write a procedure that computes 
   f by means of an iterative process.”
   Excerpt From: Harold Abelson and Gerald Jay Sussman with Julie Sussman. 
   “Structure and Interpretation of Computer Programs.” iBooks. |#

;recursive
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) 
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

;iterative
(define (ff n)
  (f-iter 2 1 0 n 0))

(define (func a b c) (+ a (* 2 b) (* 3 c)))

(define (f-iter a b c n stepnumber) 
  (cond ((< (- n 3) 0) n)
        ((= (- n 3) stepnumber) (func a b c))
        (else (f-iter (func a b c) a b n (+ stepnumber 1)))))