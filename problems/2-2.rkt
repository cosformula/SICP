#lang sicp

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment a b)
  (cons a b))

(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y)
  (cons x y))

(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define (midpoint-segment a)
  (make-point (/ 
                (+ 
                  (x-point (start-segment a))
                  (x-point (end-segment a)))
                2) 
              (/ 
                (+ 
                  (y-point (start-segment a))
                  (y-point (end-segment a)))
                2)))
              
(define x-1 (make-point 2.0 1.0))
(define x-2 (make-point 1.0 3.0))

(define a (make-segment x-1 x-2))

(print-point (midpoint-segment a))

