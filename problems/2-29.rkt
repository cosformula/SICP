#lang sicp

(define (make-mobile left right)
  (list left righy))

(define (make-branch length structure)
  (list length structure))


(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length structure)
  (car structure))


(define (branch-structure structure)
  (cdr structure))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((pair? mobile)
          (+
            (total-weight (branch-structure (left-branch mobile)))
            (total-weight (branch-structure (right-branch mobile)))
          )
        )
        (else mobile)
  )
)


(define (balance? mobile)
  (cond ((pair? mobile)
          (and
            (= 
                (* (branch-length (left-branch mobile)) (total-weight (left-branch mobile)))
                (* (branch-length (right-branch mobile)) (total-weight (right-branch mobile)))
            )
            (balance? (branch-structure (left-branch mobile)))
            (balance? (branch-structure (right-branch mobile)))
          )
        )
        (else true)
)