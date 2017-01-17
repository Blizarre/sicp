#lang scheme

(define (dec x)
  (- x 1))

(define (inc x)
  (+ x 1))

; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3
(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (dec n))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3)))))))

(define (f-iterative n)
  (cond ((< n 3) n)
        (else (f-iter-impl (- n 3) 2 1 0))))

(define (f-iter-impl remaining-n fn1 fn2 fn3)
  (define fn (+ fn1 (* 2 fn2) (* 3 fn3)))
  (if (= 0 remaining-n)
      fn
      (f-iter-impl (dec remaining-n) fn fn1 fn2)))

; pascal's triangle
;      0  1  2  3  4 (column)
;
; 0    1
; 1    1  1
; 2    1  2  1
; 3    1  3  3  1
; 4    1  4  6  4  1
;(row)


(define (pascal_triangle row col)
  (if
   (or (= row 0) (= col 0) (= col row))
   1
   (+
    (pascal_triangle (dec row) (dec col))
    (pascal_triangle (dec row) col))))