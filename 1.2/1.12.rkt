#lang sicp
; Exercise 1.12
; The following pattern of numbers is called Pascal’s triangle.
;        1
;       1 1
;      1 2 1
;     1 3 3 1
;    1 4 6 4 1
;       ...
; The numbers at the edge of the triangle are all 1, and each number inside the
; triangle is the sum of the two numbers above it. Write a procedure that
; computes elements of Pascal’s triangle by means of a recursive process.

(define (pascal row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        ((< col 0) 0)
        ((> col row) 0)
        ((+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))

(pascal 4 0)
(pascal 4 1)
(pascal 4 2)
(pascal 4 3)
(pascal 4 4)

(pascal 5 0)
(pascal 5 1)
(pascal 5 2)
(pascal 5 3)
(pascal 5 4)
(pascal 5 5)

  