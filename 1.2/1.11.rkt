#lang sicp
; Exercise 1.11
; A function f is defined by the rule that 
; f(n) = n if n < 3 and f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) if n >= 3. Write a
; procedure that computes f by means of a recursive process. Write a procedure
; that computes f by means of an iterative process.

; Recursive
(define (f-recursive n)
  (if (< n 3)
      n
      (+
       (* 1 (f-recursive (- n 1)))
       (* 2 (f-recursive (- n 2)))
       (* 3 (f-recursive (- n 3))))))

; Iterative
(define (f-iterative n)
  (if (< n 3)
      n
      (f-iter 2 1 0 1 (- n 3))))

(define (f-iter fn-1 fn-2 fn-3 counter max-count)
  (if (> counter max-count)
      (compute-fn fn-1 fn-2 fn-3)
      (f-iter
       (compute-fn fn-1 fn-2 fn-3)
       fn-1
       fn-2
       (+ 1 counter)
       max-count)))
(define (compute-fn fn-1 fn-2 fn-3)
  (+ fn-1 (* 2 fn-2) (* 3 fn-3)))

(f-recursive 9)
(f-iterative 9)

(f-recursive 3)
(f-iterative 3)

(f-recursive 4)
(f-iterative 4)

(f-recursive 5)
(f-iterative 5)

(f-recursive 2)
(f-iterative 2)

(f-recursive 1)
(f-iterative 1)

(f-recursive 0)
(f-iterative 0)

(f-recursive -1)
(f-iterative -1)
