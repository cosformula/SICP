#lang sicp

;;; add-1

(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))

(+ 2 3)

;;; 1. (inc (+ (1) 3))
;;; 2. (inc (inc 3))
;;; 3. (inc 4)
;;; 4. 5


;;; add-2

(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

(+ 2 3)

;;; 1. (+ 1 4)
;;; 2. (+ 0 5)
;;; 3. 5