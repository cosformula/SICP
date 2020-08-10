#lang sicp

(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (enumerate-interval i n)
  (if (> i n)
      nil
      (cons i (enumerate-interval (+ i 1) n))
  )
)

(define (accumulate op inital sequence)
  (if (null? sequence)
      inital
      (op (car sequence)
          (accumulate op inital (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
              (flatmap (lambda (j) 
                        (map (lambda (k) (list i j k))
                          (filter (lambda (x) (and (not (= x j)) (not (= x i)))) (enumerate-interval 1 n))))
                      (filter (lambda (x) (not (= x i))) (enumerate-interval 1 n)))
            )
            (enumerate-interval 1 n)))


(define (filter fillfuled? items)
    (cond ((null? items) '())
          ((fillfuled? (car items)) (cons (car items) (filter fillfuled? (cdr items))))
          (else (filter fillfuled? (cdr items)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (caddr pair)  (+ (car pair) (cadr pair) (caddr pair) )))

(define (s-sum-pairs n s)
  (map make-pair-sum
      (filter (lambda (pair) (= s (+ (car pair) (cadr pair) (caddr pair))))
              (unique-pairs n))))

(s-sum-pairs 6 10)