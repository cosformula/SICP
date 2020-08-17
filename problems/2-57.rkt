#lang sicp


;;; exp


(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (** b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) 
  (cond ((> (length s) 3) (list '+ (caddr s) (cadddr s)))
        (else (caddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (cond ((> (length p) 3) (list '* (caddr p) (cadddr p)))
        (else (caddr p))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base p) (cadr p))

(define (exponent p) (caddr p))

(define (make-exponentiation base n)
  (cond ((or (=number? base 1) (=number? base 0)) 1)
        ((=number? n 0) 1)
        ((=number? n 1) base)
        ((and (number? base) (number? n)) (** base n))
        (else (list '** base n))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
          (make-product (exponent exp)
                        (make-product 
                          (make-exponentiation (base exp) (- (exponent exp) 1))
                          (deriv (base exp) var))))
        (else
          (error "unkown expression type -- DERIV" exp))))

(deriv '(+ x 3 x 1) 'x)

(deriv '(* x y (+ x 3)) 'x)