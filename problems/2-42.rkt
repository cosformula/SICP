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

(define empty-board nil)

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define (safe? k positions)
  (if (< k 2)
    true
    (and 
      (and
        (not (= (car positions) (last positions)))
        (not (= (+ (car positions) (- k 1)) (last positions)))
        (not (= (- (car positions) (- k 1)) (last positions)))
      )
      (safe? (- k 1) (cdr positions)))
  )
)



(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)