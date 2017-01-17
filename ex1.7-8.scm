#lang racket

(define (average3 x y z)
  (/ (+ x y z) 3))

(define (average2 x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x)
  (* x x))

(define (L1-distance x y)
  (abs (- x y)))

(define (good-enough? guess next-guess)
  (< (/ (L1-distance next-guess guess) next-guess) 0.00001))

(define (my-newton-internal improve-function guess x)
  (define next-guess (improve-function guess x)) 
  (if (good-enough? guess next-guess)
      next-guess
      (my-newton-internal improve-function next-guess x)))

(define (square-root x)
  (my-newton-internal
   (lambda (guess x)
     (average2 (/ x guess) guess))
   1.0 x))

(define (cube-root x)
  (my-newton-internal
   (lambda (guess x)
     (average3 (/ x (square guess)) guess guess))
   1.0 x))

(square-root 16)

(cube-root 27)
