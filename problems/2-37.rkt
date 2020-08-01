#lang sicp

(define (accumulate op inital sequence)
  (if (null? sequence)
      inital
      (op (car sequence)
          (accumulate op inital (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

(define vector (list 1 2 3 4))

(define matrix (list (list 1 2 3 4)  (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(dot-product vector vector)
(matrix-*-vector matrix vector)
(matrix-*-matrix matrix matrix)
(transpose matrix)


