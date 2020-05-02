#lang sicp

;;; sqrt

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

;;; new good-enough?

(define (good-enough? guess next-guess)
  (< (abs (- guess next-guess)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
    guess
    (sqrt-iter (improve guess x)
                x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; sqrt test
(sqrt 9)

;;; same precision on large number
(sqrt 1000000000000)

;;; same precision on small number
(sqrt 0.0001)

