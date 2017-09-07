#lang sicp
; Exercise 1.26
; Louis Reasoner is having great difficulty doing Exercise 1.24. His
; fast-prime? test seems to run more slowly than his prime? test. Louis calls
; his friend Eva Lu Ator over to help. When they examine Louis’s code, they
; find that he has rewritten the expmod procedure to use an explicit
; multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base 
             (expmod base (- exp 1) m))
          m))))

; I don’t see what difference that could make,” says Louis. “I do.” says Eva.
; “By writing the procedure like that, you have transformed the Θ(log n)
; process into a Θ(n) process.” Explain.

; There used to be only 1 call to expmod in each branch of the cond, except the
; terminating condition, hence the process was a linear recursive process. Now
; Louis added another call to expmod, making expmod tree recursive making the
; execution time grow exponentially, 2^n, with the depth of the tree. The depth
; of the tree is log(n), hence 2^log(n) = n is the order of growth.