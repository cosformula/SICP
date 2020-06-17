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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
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


(timed-prime-test 1000000007)
(timed-prime-test 10000000019)
;;; computers are fast, use large number to get meaningful results.
(search-for-primes 10000000000)
(search-for-primes 100000000000)
(search-for-primes 1000000000000)
(search-for-primes 10000000000000)


;;; results
;;; 10000000000  *** 2103
;;; 100000000000  *** 4013
;;; 1000000000000  *** 13059
;;; 10000000000000  *** 37471