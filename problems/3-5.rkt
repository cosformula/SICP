#lang sicp

;;; 3.5

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
            (/ trials-passed trials))
          ((experiment)
            (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (monte-carlo trials (lambda () (p (random-in-range x1 x2) (random-in-range y1 y2)))))

(define (estimate-pi trials)
  (* (estimate-integral (lambda (x y) (<= (+ (* x x) (* y y)) 1.0)) -1.0 1.0 -1.0 1.0 trials) 4.0))

(estimate-pi 1000000)

;;; 3.6
(define rand msg
  (let ((val random-init))
    (cond ((eq? msg 'generate)
            (begin (set! val (rand-update val) val)))
          ((eq? msg 'reset)
            (lambda (x) (set! val x))))))
  