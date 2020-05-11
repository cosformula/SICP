#lang sicp

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (* a b)
  (multiply-iter a b 0))


(define (multiply-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (multiply-iter (double a) (halve b) c))
        (else (multiply-iter a (- b 1) (+ c a)))))

(* 2 3)
(* 3 3)
(* 3 1)
(* 3 0)
(* 0 0)
(* 0 3)





