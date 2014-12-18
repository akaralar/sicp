#lang planet neil/sicp

#| 
Exercise 1.12. The following pattern of numbers is called Pascal's triangle.

       1
      1 1
     1 2 1
    1 3 3 1
   1 4 6 4 1
      ...

The numbers at the edge of the triangle are all 1, and each number inside the 
triangle is the sum of the two numbers above it. Write a procedure that computes
elements of Pascal's triangle by means of a recursive process.

Excerpt From: Harold Abelson and Gerald Jay Sussman with Julie Sussman. 
“Structure and Interpretation of Computer Programs.” iBooks.
|#

(define (pascal x y)
  (cond ((= x 0) 1)
        ((= x y) 1)
        ((< x 0) 0)
        ((> x y) 0)
        (else (+ (pascal x (- y 1))
                 (pascal (- x 1) (- y 1))))))

