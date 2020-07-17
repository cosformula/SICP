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


(define (deep-reverse items)
  (define (iter items result)
      (if (null? items)
        result
        (if (pair? items)
          (iter (cdr items) (cons (iter (car items) nil) result))
          items)))
 (iter items nil))

(define x (list (list 1 2) (list 3 4)))

x
(reverse x)

(deep-reverse x)