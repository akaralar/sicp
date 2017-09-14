#lang sicp
; Exercise 1.34
; Suppose we define the procedure

(define (f g) (g 2))

; Then we have
(define (square x) (* x x))
(f square)


(f (lambda (z) (* z (+ z 1))))

(f f)
; What happens if we (perversely) ask the interpreter to evaluate the
; combination (f f)? Explain.
; -----
; It would try to apply (f f) -> (f 2) -> (2 2) which would give an error
; because first term is not a procedure

