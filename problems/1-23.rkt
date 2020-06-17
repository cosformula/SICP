#lang sicp

(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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


(timed-prime-test 1000000007)
(timed-prime-test 10000000019)


;;; results

;;; before
;;; 1000000007  *** 238
;;; 10000000019  *** 612

;;; after
;;; 1000000007  *** 410
;;; 10000000019  *** 1175

;;; see also: https://codology.net/post/sicp-solution-exercise-1-23/