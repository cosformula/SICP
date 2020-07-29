#lang sicp

(define (accumulate op inital sequence)
  (if (null? sequence)
      inital
      (op (car sequence)
          (accumulate op inital (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) 
                (cons (p x) y)
              )
              nil
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ y 1)
              )
              0
              sequence))


(map (lambda (x) (* x x)) (list 1 2 3))
(append (list 1 2) (list 3 4))
(length (list 1 2 3 4))