#lang sicp

(define (fringe items)
  (define (iter items result)
      (if (null? items)
        result
        (if (pair? items)
          (iter (car items) (iter (cdr items) result))
          (cons items result))))
 (iter items nil))

(define x (list (list 1 2) (list 3 4)))                                 

(fringe x)

(fringe (list x x))

