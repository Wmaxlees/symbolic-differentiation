#lang racket

; Most of this code is based on M. Doug Williams' graph-search.rkt file

(provide define-ruleset
         define-rule
         ruleset-rules
         applicable?
         action
         rule-name)

(struct ruleset (name (rules #:mutable))
  #:transparent)

(define-syntax-rule (define-ruleset name)
  (define name (ruleset 'name '())))

(struct rule (name ruleset operator action)
  #:transparent)

(define (applicable? rule exp var)
  ((rule-operator rule) exp var))

(define (action rule exp var)
  ((rule-action rule) exp var))

(define-syntax define-rule
  (syntax-rules (=>)
    ((define-rule ruleset (name arg ...)
       operator
       =>
       action)
     (set-ruleset-rules!
      ruleset
      (append (ruleset-rules ruleset)
              (list (rule 'name
                          ruleset
                          (lambda (arg ...) operator)
                          (lambda (arg ...) action))))))))