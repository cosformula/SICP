#lang sicp

;;; sqrt

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (square x)
  (* x x))

;;; impove-guess = (x/y^2 + 2y)/3

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

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
(sqrt 27)

;;; same precision on large number
(sqrt 1000000000000)

;;; same precision on small number
(sqrt 0.0001)

