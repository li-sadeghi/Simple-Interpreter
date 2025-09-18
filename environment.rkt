#lang racket

(require "datatypes.rkt")
(require (only-in (lib "eopl.ss" "eopl") eopl:error cases))

(define the-env 'uninitialized)

(define initialize-env!
  (lambda ()
    (set! the-env (init-env))))

(define (get-env) the-env)

(define (set-env! env)
  (set! the-env env))

(define init-env
  (lambda ()
    (empty-environment)))

(define extend-env
  (lambda (var val env)
    (extend-environment var val env)))

(define apply-env
  (lambda (var env)
    (cases environment env
      (empty-environment () #f)
      (extend-environment (saved-var val saved-env)
        (if (equal? var saved-var)
            val
            (apply-env var saved-env))))))

(define (apply-env-with-type var env)
  (cases environment env
    (empty-environment () #f)
    (extend-environment (saved-var val saved-env)
      (if (equal? var saved-var)
          (apply-env var saved-env)
          (apply-env-with-type var saved-env)))))

(provide (all-defined-out))
