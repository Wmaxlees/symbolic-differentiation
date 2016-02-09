#lang racket

(provide add-rule
         add-simplifier
         rule-exists-for?
         exec-rule
         simplify)

(define rule-table (make-hash))
(define simplification-table (make-hash))

(define-syntax add-rule
  (syntax-rules (->)
    ((_ (arg ...)
      symbol
      ->
      action)
     (hash-set! rule-table
                symbol
                (λ (arg ...) action)))))

(define-syntax add-simplifier
  (syntax-rules (->)
    ((_ (arg ...)
        symbol
        ->
        action)
     (if (hash-has-key? simplification-table symbol)
         (hash-set! simplification-table symbol (append (hash-ref simplification-table symbol) (list (λ (arg ...) action))))
         (hash-set! simplification-table symbol (list (λ (arg ...) action)))))))

(define (rule-exists-for? exp)
  (hash-has-key? rule-table (car exp)))

(define (exec-rule exp var)
  ((hash-ref rule-table (car exp)) exp var))

(define (simplify exp)
  (cond [(hash-has-key? rule-table (car exp))
         (for ([i (hash-ref simplification-table (car exp))])
           (set! exp (i exp)))])
  exp)