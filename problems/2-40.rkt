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

(define (unique-pairs n)
  (accumulate 
              append
              nil
              (map (lambda (i)
                      (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1)))
                    )
                    (enumerate-interval 1 n))))

(enumerate-interval 1 0)
(unique-pairs 6)

(define (filter fillfuled? items)
    (cond ((null? items) '())
          ((fillfuled? (car items)) (cons (car items) (filter fillfuled? (cdr items))))
          (else (filter fillfuled? (cdr items)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
        (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
      (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)