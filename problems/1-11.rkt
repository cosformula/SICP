#lang sicp

;;; 
(define (f n)
  (if (< n 3) 
    n
    (+ 
      (+ 
        (f (- n 1))
        (* 
          2
          (f (- n 2))))
      (* 3 
        (f (- n 3))))))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)

;;; iter
(define (fn-iter a b c n)
  (if (= n 0)
    a
    (fn-iter 
      b
      c
      (+ 
        (+ c 
          (* 2 b))
        (* 3 a))
      (- n 1))))

(define (fn n)
  (fn-iter 0 1 2 n))

(fn 0)
(fn 1)
(fn 2)
(fn 3)
(fn 4)
(fn 5)

