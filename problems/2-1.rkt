#lang sicp

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ (if (< d 0) (- 0 n) n) g) (/ (abs d) g))))

(print-rat (make-rat -4 6))