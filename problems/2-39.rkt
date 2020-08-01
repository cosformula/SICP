#lang sicp

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (cons x nil))) nil sequence))

(define (reverse-left sequence)
  (fold-left (lambda (r c) (cons c r)) nil sequence))

(reverse-right (list 1 2 3 4))
(reverse-left (list 1 2 3 4))

