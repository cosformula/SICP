#lang sicp

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
  (make-interval (- center (* center percent) (+ center (* center percent)))))

(define (percent interval)
  (/ (- (upper-bound interval) (center interval)) (width interval))
