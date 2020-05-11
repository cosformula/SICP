#lang sicp

;;; Ackermann Function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                (A x (- y 1))))))

(A 1 10)
;;; 1024
(A 2 4)
;;; 65536
(A 3 3)
;;; 65536

(define (f n) (A 0 n))
;;; f = 2 * n

(define (g n) (A 1 n))
;;; g(n) = 2^n

(define (h n) (A 2 n))
;;; h(1) = 2^1
;;; h(2) = 2^2
;;; h(3) = 2^(2^2)
;;; h(4) = 2^(2^(2^2))
