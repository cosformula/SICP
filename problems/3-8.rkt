#lang sicp

;;; 3.8

(define f
  (let ((state 1))
    (lambda (x)
      (set! state (* state x))
        state
    ))
)


(+ (f 1) (f 0))

(+ (f 0) (f 1))
