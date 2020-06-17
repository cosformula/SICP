#lang sicp

;;; (define (cont-frac n d k)
;;;   (define (cont-frac-iter n d i)
;;;     (if (= k i)
;;;         (/ (n i) (+ (d i))
;;;         (/ (n i) (+ (d i) (cont-frac-iter n d (+ i 1))))))
;;;   (cont-frac-iter n d 1))
  

(define (cont-frac n d k)
  (define (cont-frac-iter n d i result)
    (if (= i 0)
      result
      (cont-frac-iter n d (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter n d k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)
