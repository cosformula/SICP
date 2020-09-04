#lang sicp

;;; 3.7

(define (make-account balance secret)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch s m)
    (if (eq? s secret)
        (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT")))
        (error "Incorrect password"))
    )
  dispatch)

(define (make-joint account secrect new-secrect)
  (define (dispatch s m)
    (if (eq? s new-secrect)
      (account secrect m)
      (error "Incorrect password")))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))

((peter-acc 'open-sesame 'withdraw) 40)

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 40)
