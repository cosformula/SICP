#lang sicp

(define (accumulate op inital sequence)
  (if (null? sequence)
      inital
      (op (car sequence)
          (accumulate op inital (cdr sequence)))))

(define (count-leaves t)
  (accumulate 
    (lambda (x y) (+ x y)) 
    0
    (map (lambda (t)
            (cond ((null? t) 0)
                  ((pair? t) (count-leaves t))
                  (else 1)
            ))
    t)))


(count-leaves (list 1 2 (list 1 2) (list 3 4 5)))
