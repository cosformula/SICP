#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

(define (same-parity . w)
  (define (filter fillfuled? items)
    (cond ((null? items) '())
          ((fillfuled? (car items)) (cons (car items) (filter fillfuled? (cdr items))))
          (else (filter fillfuled? (cdr items)))))

  (cond ((null? w) '())
        ((odd? (car w)) (filter odd? w))
        ((even? (car w)) (filter even? w))))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
