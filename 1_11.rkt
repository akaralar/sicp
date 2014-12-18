#lang planet neil/sicp

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) 
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(define (ff n)
  (f-iter 2 1 0 n 0))

(define (f-iter a b c n count) 
  (cond ((< (- n 3) 0) n)
        ((= (- n 3) count) (func a b c))
        (else (f-iter (func a b c)
                     a
                     b
                     n
                     (+ count 1)))))

(define (func a b c) (+ a (* 2 b) (* 3 c)))
  