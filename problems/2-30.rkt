#lang sicp


;;; (define (square-list items)
;;;   (if (null? items)
;;;       '()
;;;       (cons (* (car items) (car items)) (square-list (cdr items)))))


(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons
                  (square-tree (car tree))
                  (square-tree (cdr tree))))))


;;; (define (square-tree tree)
;;;   (map (lambda (sub-tree)
;;;           (if (pair? sub-tree)
;;;               (square-tree sub-tree)
;;;               (* sub-tree sub-tree))) tree))

(square-tree
  (list 1
    (list 2 (list 3 4) 5)
              (list 6 7)))