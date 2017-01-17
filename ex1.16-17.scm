#lang scheme

(define (dec x)
  (- x 1))

; exercise 1.16

(define (fast-expt b n)
  (fast-expt-internal 1 b n))

(define (fast-expt-internal a b n)
  (cond ((= n 0) a)
        ((is-even? n) (fast-expt-internal a (* b b) (/ n 2)))
        (else (fast-expt-internal (* a b) b (dec n)))))

; (fast-expt 2 8) => 256

; exercise 1.17

(define (is-even? n)
  (= (remainder n 2) 0))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((is-even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (dec b))))))

; (fast-mul 2 185) => 370