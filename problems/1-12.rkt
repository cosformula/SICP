#lang sicp

;;; 
(define (p a b)
  (cond ((= b 0) 1)
        ((= a b) 1)
        (else (+ 
                (p (- a 1) (- b 1))
                (p (- a 1) b)))))

(p 4 0)
(p 4 1)
(p 4 2)
(p 4 3)
(p 4 4)
