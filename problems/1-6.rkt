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

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
                x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; sqrt test
(sqrt 9)

;;; low precision on large number
(sqrt 1000000000000)

;;; low precision on small number
(sqrt 0.0001)

;;; Alyssa's new if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;;; new if test
(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter-new (improve guess x)
                  x)))

(define (sqrt-new x)
  (sqrt-iter-new 1.0 x))

;;; new sqrt test

(sqrt-new 9)
;;; infinite loop