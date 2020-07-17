#lang sicp


(define (for-each action items)
  (cond ((not (null? items))
        (action (car items))
        (for-each action (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))