#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (cond ((> (lower-bound x) 0) 
              (cond ((> (lower-bound y) 0) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
                    ((> (upper-bound y) 0) (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
                    (else (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))))
        ((> (upper-bound x) 0)
              (cond ((> (lower-bound y) 0) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
                    ((> (upper-bound y) 0) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
                    (else (make-interval (min (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))
                                         (min (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))))
        (else
              (cond ((> (lower-bound y) 0) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
                    ((> (upper-bound y) 0) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
                    (else (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))))))

(define (div-interval x y)
  (mul-interval x
                (if (< (upper-bound y) 0)
                    (make-interval (/ 1.0 (lower-bound y))
                               (/ 1.0 (upper-bound y)))
                    (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define x (make-interval -1 2) )
(define y (make-interval -2 3) )

(add-interval x y)
(sub-interval x y)
(mul-interval x y)
(div-interval x y)

