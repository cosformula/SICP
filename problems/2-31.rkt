#lang sicp

(define (square item)
  (* item item))

(define (tree-map fn tree)
  (map (lambda (tree)
          (if (pair? tree)
              (square-tree tree)
              (fn tree))) tree))

(define (square-tree tree) (tree-map square tree))

(square-tree
  (list 1
    (list 2 (list 3 4) 5)
              (list 6 7)))