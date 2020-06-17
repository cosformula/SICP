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


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
        (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                      m))))

                      

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
  (if (fast-prime? n 10000)
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


(timed-prime-test 659047)
(timed-prime-test 6589981)
(timed-prime-test 65899073)
(timed-prime-test 658990067)
(timed-prime-test 1000000007)


;;; results
;;; 10000000000  *** 2103
;;; 100000000000  *** 4013
;;; 1000000000000  *** 13059
;;; 10000000000000  *** 37471