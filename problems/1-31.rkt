#lang sicp

(define (product-1 term a next b)
  (if (> a b)
      1
      (* (term a)
        (product-1 term (next a) next b))))

(define (product-2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity a)
  a)

(define (inc a)
  (+ a 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (factorial n)
  (product-1 identity 1 inc n))

(factorial 0)
(factorial 2)
(factorial 3)

(define (pi n)
  (define (term a)
    (if (even? a)
        (/ (+ a 2.0) (+ a 1))
        (/ (+ a 1.0) (+ a 2))))
  (* 4.0 (product-1 term 1 inc n)))

(pi 10000000)