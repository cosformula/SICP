#lang sicp

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (double n)
  (+ n n))

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

;;; compute p q
(define (np p q)
  (+ (square p) (square q)))

(define (nq p q)
  (+ (square q) (* 2 p q)))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
          (fib-iter a
                    b
                    (np p q)
                    (nq p q)
                    (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)







