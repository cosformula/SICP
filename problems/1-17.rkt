#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
        (else (+ a (* a (- b 1))))))

(* 2 3)
(* 3 3)
(* 3 1)
(* 3 0)
(* 0 0)
(* 0 3)





