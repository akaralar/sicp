#lang sicp
; Exercise 1.31
; 1. The sum procedure is only the simplest of a vast number of similar
;    abstractions that can be captured as higher-order procedures. Write an
;    analogous procedure called product that returns the product of the values
;    of a function at points over a given range. Show how to define factorial in
;    terms of product. Also use product to compute approximations to π using the
;    formula
;
;    π/4 = (2*4*4*6*6*8*...)/(3*3*5*5*7*7*...)
;
; 2. If your product procedure generates a recursive process, write one that
;    generates an iterative process. If it generates an iterative process,
;    write one that generates a recursive process.
; -----
; 1.
(define (product f a next b)
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))

; For factorial, we define an identity procedure
(define (identity x) x)

; And an incrementing function to give next values
(define (inc x) (+ x 1))

(define (factorial x)
  (product identity 1 inc x))
(factorial 6)

; We define Wallis formula with number of terms to calculate and the product
; procedure to use as parameters to the procedure. The product procedure
; parameter is useful in the second part of the question, where we write another
; product formula with a different process
(define (wallis-pi n product-procedure)
  ; This will give nominator values for Wallis Formula
  (define (wallis-nominator k)
    (cond ((= k 0) 2.0)
          ((= (remainder k 2) 0) (wallis-nominator (- k 1)))
          (else (+ 2 (wallis-nominator (- k 1))))))
  ; This will give denominator values for Wallis Formula
  (define (wallis-denominator k)
    (cond ((= k 0) 3.0)
          ((= (remainder k 2) 0) (+ 2 (wallis-denominator (- k 1))))
          (else (wallis-denominator (- k 1)))))
  ; This will give the kth term of the Wallis Formula
  (define (term k) (/ (wallis-nominator k) (wallis-denominator k)))
  (product-procedure term 0 inc n))

;                     π/4 = .785398163
(wallis-pi 100 product)   ; .7815948495107451
(wallis-pi 1000 product)  ; .7850067365262514
(wallis-pi 10000 product) ; .7853589062479106

; 2.
; Our product procedure generates a recursive process, so we need to write an
; iterative process
(define (product-iter f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (f a) result))))
  (iter a 1))

(define (factorial-iter x)
  (product-iter identity 1 inc x))
(factorial-iter 6)

;                          π/4 = .785398163
(wallis-pi 100 product-iter)   ; .7815948495107449
(wallis-pi 1000 product-iter)  ; .7850067365262512
(wallis-pi 10000 product-iter) ; .7853589062479146

