#lang sicp

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (reverse items)
  (define (iter items result)
    (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
 (iter items nil))


;;;  (define (reverse items)
;;;   (if (null? (cdr items))
;;;       (car items)
;;;       (append (reverse (cdr items)) (cons (car items) nil))))

(reverse (list 1 4 9 16 25))