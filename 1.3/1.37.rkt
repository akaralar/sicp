#lang sicp
; Exercise 1.37
; 1. An infinite continued fraction is an expression of the form
;
;    f = N1 / (D1 + (N2 / (D2 + (N3 / (D3 +....
;
;    As an example, one can show that the infinite continued fraction expansion
;    with the Ni and the Di all equal to 1 produces 1/φ, where φ is the golden
;    ratio (described in 1.2.2). One way to approximate an infinite continued
;    fraction is to truncate the expansion after a given number of terms. Such a
;    truncation—a so-called finite continued fraction k-term finite continued
;    fraction—has the form
;
;    N1 / (D1 + (N2 / ... + (Nk / Dk)
;
;    Suppose that n and d are procedures of one argument (the term index i) that
;    return the Ni and Di of the terms of the continued fraction. Define a
;    procedure cont-frac such that evaluating (cont-frac n d k) computes the
;    value of the k-term finite continued fraction. Check your procedure by
;    approximating 1/φ using

;    (cont-frac (lambda (i) 1.0)
;               (lambda (i) 1.0)
;               k)
;
;    for successive values of k. How large must you make k in order to get an
;    approximation that is accurate to 4 decimal places?
;
; 2. If your cont-frac procedure generates a recursive process, write one that
;    generates an iterative process. If it generates an iterative process,
;    write one that generates a recursive process.
; -----
; 1.
(define (cont-frac n d k)
  (define (frac x)
    (if (= x k)
        (/ (n x) (d x))
        (/ (n x) (+ (d x) (frac (+ x 1))))))
  (frac 1))

; function parameter is the cont-frac to use, will be useful in the second part
(define (infinite-cont function tolerance)
  (define (try k)
    ; φ = 1 / cont-frac
    (/ 1.0 (function (lambda (i) 1.0) (lambda (i) 1.0) k)))
  (define (close-enough? v1 v2)
    (< (abs (- v2 v1)) tolerance))
  (define phi (/ (+ 1.0 (sqrt 5)) 2.0))
  (define (iter a)
    (newline)
    (display "Trying ")
    (display a)
    (cond ((close-enough? (try a) phi)
           (newline)
           (display "Found: ")
           (display a)
           (newline)
           (try a))
          (else (iter (+ a 1)))))
  (iter 1))

; To find value accurate to 4 decimal places, tolerance should be 0.00005
(define tolerance 0.00005)
(infinite-cont cont-frac tolerance)
; k needs to be 12 
;
; 2.
; Our procedure generates a recursive process, so we need to write an iterative
; process

(define (cont-frac-iter n d k)
  (define (iter x result)
    (if (= x 0)
        result
        (iter (- x 1) (/ (n x) (+ (d x) result)))))
  (iter k 0))

(infinite-cont cont-frac-iter tolerance)



