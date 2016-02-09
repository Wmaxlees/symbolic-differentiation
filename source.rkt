#lang racket

(require "rules.rkt")

(add-rule (exp var)
          '+
          ->
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))

(add-rule (exp var)
          '*
          ->
          (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))

(add-rule (exp var)
          '**
          ->
          (make-exponentiation
           (exponent exp)
           (base exp)
           (- (exponent exp) 1)))

(add-simplifier (exp)
                '+
                ->
                (if (and (sum? exp) (=number? (addend exp) 0))
                    (augend exp)
                    exp))

(add-simplifier (exp)
                '+
                ->
                (if (and (sum? exp) (=number? (augend exp) 0))
                    (addend exp)
                    exp))

(add-simplifier (exp)
                '+
                ->
                (if (and (sum? exp) (number? (addend exp)) (number? (augend exp)))
                    (+ (addend exp) (augend exp))
                    exp))

(add-simplifier (exp)
                '*
                ->
                (if (and (product? exp) (or (=number? (multiplier exp) 0) (=number? (multiplicand exp) 0)))
                    0
                    exp))

(add-simplifier (exp)
                '*
                ->
                (if (and (product? exp) (=number? (multiplier exp) 1))
                    (multiplicand exp)
                    exp))

(add-simplifier (exp)
                '*
                ->
                (if (and (product? exp) (=number? (multiplicand exp) 1))
                    (multiplier exp)
                    exp))

(add-simplifier (exp)
                '*
                ->
                (if (and (product? exp) (number? (multiplier exp)) (number? (multiplicand exp)))
                    (* (multiplier exp) (multiplicand exp))
                    exp))

(add-simplifier (exp)
                '*
                ->
                (if (and (exponentiation? exp) (=number? (exponent exp) 1))
                    (base exp)
                    exp))

(add-simplifier (exp)
                '*
                ->
                (if (and (exponentiation? exp) (=number? (exponent exp) 0))
                    1
                    exp))
                                                
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [else
         (cond [(rule-exists-for? exp) (exec-rule exp var)]
               [else (error "unknown expression type -- DERIV" exp)])]))

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
  (simplify (list '+ a1 a2)))

(define (make-product m1 m2)
  (simplify (list '* m1 m2)))

(define (make-exponentiation e0 e1 e2)
  (list '** (list '* e0 e1) e2))

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
