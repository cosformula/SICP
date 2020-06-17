#lang sicp

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
      (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc a)
  (+ a 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
    dx))

(define (integral-simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (co k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (y k)
    (* (co k) (f (+ a (* k h)))))
  (* (/ h 3.0) (sum y 1 inc n)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral-simpson cube 0 1 100)
(integral-simpson cube 0 1 1000)
