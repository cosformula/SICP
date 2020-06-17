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

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
  

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "  *** ")
  (display elapsed-time))


(define (search-for-primes-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (search-for-primes-test (+ n 1) start-time)))

(define (search-for-primes n)
  (newline)
  (display n)
  (search-for-primes-test n (runtime)))


(timed-prime-test 199)

(timed-prime-test 659047)
(timed-prime-test 6589981)
(timed-prime-test 65899073)
(timed-prime-test 658990067)
(timed-prime-test 1000000007)

;;; it takes long time to calculate
;;; fast-expt result could be a large number that slow to do remiander