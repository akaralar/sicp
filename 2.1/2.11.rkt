#lang sicp
; Exercise 2.11
; In passing, Ben also cryptically comments: “By testing the signs of the
; endpoints of the intervals, it is possible to break mul-interval into nine
; cases, only one of which requires more than two multiplications.” Rewrite
; this procedure using Ben’s suggestion.
; -----
; Considering signs of endpoints for an interval, there are 3 cases:
; 0. (lower: +, upper: +)
; 1. (lower: -, upper: +)
; 2. (lower: -, upper: -)
; We can define a test for cases of an interval that returns the case number:
(define (endpoint-signs-case interval)
  (let ((lower (lower-bound interval))
        (upper (upper-bound interval)))
    (cond ((and (positive? lower) (positive? upper)) 0)
          ((and (negative? lower) (negative? upper)) 2)
          (else 1))))
           
; Since we are multiplying 2 intervals, 3 * 3 = 9 cases in total
; 0. (0, 0)
; 1. (0, 1)
; 2. (0, 2)
; 3. (1, 0)
; 4. (1, 1)
; 5. (1, 2)
; 6. (2, 0)
; 7. (2, 1)
; 8. (2, 2)
; We can define a procedure to encapsulate this
(define (interval-signs-case interval-1 interval-2)
  (let ((case-1 (endpoint-signs-case interval-1))
        (case-2 (endpoint-signs-case interval-2)))
    (+ (* case-1 3) case-2)))

; Now define multiply with these 9 cases
(define (mul-interval-2 x y)
  (let ((signs (interval-signs-case x y))
        (lower-x (lower-bound x))
        (upper-x (upper-bound x))
        (lower-y (lower-bound y))
        (upper-y (upper-bound y)))
    (cond ((= signs 0)
           (make-interval (* lower-x lower-y) (* upper-x upper-y)))
          ((= signs 1)
           (make-interval (* upper-x lower-y) (* upper-x upper-y)))
          ((= signs 2)
           (make-interval (* upper-x lower-y) (* lower-x upper-y)))
          ((= signs 3)
           (make-interval (* lower-x upper-y) (* upper-x upper-y)))
          ((= signs 4)
           (make-interval (min (* lower-x upper-y) (* upper-x lower-y))
                          (max (* lower-x lower-y) (* upper-x upper-y))))
          ((= signs 5)
           (make-interval (* upper-x lower-y) (* lower-x lower-y)))
          ((= signs 6)
           (make-interval (* lower-x upper-y) (* upper-x lower-y)))
          ((= signs 7)
           (make-interval (* lower-x upper-y) (* lower-x lower-y)))
          ((= signs 8)
           (make-interval (* upper-x upper-y) (* lower-x lower-y))))))

(#%require "2.7.rkt")
(#%require "2.8.rkt")


; To test the results, we will create interval generators
(define (lower-bound-for sign)
  (cond ((= sign 0) 0)
        ((= sign 1) -15)
        ((= sign 2) -10)))
(define (upper-bound-for sign)
  (cond ((= sign 0) 8)
        ((= sign 1) 6)
        ((= sign 2) -9)))
(define (interval-for sign)
  (make-interval (lower-bound-for sign) (upper-bound-for sign)))

; We also need to have a way to check whether two intervals are equal
(define (equal-interval? interval-1 interval-2)
  (and (= (lower-bound interval-1) (lower-bound interval-2))
       (= (upper-bound interval-1) (upper-bound interval-2))))

; We create required intervals for a case, multiply them with the first method
; and the method above, and compare results to see whether they are equal
(define (test-multiplication? signs)
  (let ((interval-1 (interval-for (quotient signs 3)))
        (interval-2 (interval-for (remainder signs 3))))
    (if (or (negative? signs) (> signs 8))
        false
        (equal-interval? (mul-interval interval-1 interval-2)
                        (mul-interval-2 interval-1 interval-2)))))

; We test all cases iteratively with a counter
(define (test-all-cases current)
  (cond ((test-multiplication? current)
         (display "Success at case: ")
         (display current)
         (newline)
         (test-all-cases (+ 1 current)))
        ((= current 9)
         (display "All correct!")
         (newline))
        (else 
         (display "Failed at case: ")
         (display current)
         (newline))))
(test-all-cases 0)

; We see "All correct" in the end, so our implementation is most probably
; correct