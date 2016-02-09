#lang racket

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [(exponentiation? exp)
         (make-exponentiation (base exp)
                              (exponent exp))]
        [else
         (error "unknown expression type -- DERIV" exp)]))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list '+ a1 a2)]))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]))

(define (make-exponentiation m1 m2)
  (cond [(=number? m2 1) 1]
        [(=number? m2 2) (list '* 2 m1)]
        [else (list '** (list '* m2 m1) (- m2 1))]))

(define (addend s) (cadr s))
(define (augend s)
  (cond
    [(> (length s) 3) (append '(+) (list-tail s 2))]
    [else (caddr s)]))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (cond
    [(> (length p) 3) (append '(*) (list-tail p 2))]
    [else (caddr p)]))    

(define (base e) (cadr e))
(define (exponent e) (caddr e))
