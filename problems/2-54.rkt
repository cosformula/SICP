#lang sicp


(define (equal? a b)
  (if (pair? a)
      (if (pair? b)
        (and (equal? (car a) (car b))
             (equal? (cdr a) (cdr b)))
        false)
      (if (pair? b)
          false
          (eq? a b))))

(equal? 'a 'b)
(equal? 'a 'a)
(equal? '((x1 x2) (y1 y2)) '((x1 x2) (y1 y2)))
(equal? '((x1 x2) (y1 y2)) '((x1 x2) (y1 y3)))
