#lang sicp


;;; 3.1
(define (make-accumulator init)
  (let ((balance init))
    (lambda (amount)
      (begin (set! balance (+ balance amount)) balance))))

(define A (make-accumulator 5))

(A 10)

(A 10)


;;; 3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (param)
      (cond ((eq? param 'how-many-calls?) count)
            ((eq? param 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1)) (f param)))))))



(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)


;;; 3.3

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

(define acc-1 (make-account 100 'test))

((acc-1 'test 'withdraw) 40)

;;; ((acc-1 'test-1 'withdraw) 40)

;;; 3.4

(define call-cops 'COPS)

(define (make-account-2 balance secret)
  (let ((count 0))
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
          (begin 
                  (set! count 0)
                  (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    (else (error "Unknown request -- MAKE-ACCOUNT"))))
          (begin  (set! count (+ count 1))
                  (if (>= count 7)
                    (call-cops))
                  (error "Incorrect password")))
      )
    dispatch))
  

(define acc-2 (make-account 100 'test))

((acc-2 'test 'withdraw) 40)

((acc-2 'test-1 'withdraw) 40)