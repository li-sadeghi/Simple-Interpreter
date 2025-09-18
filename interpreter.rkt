#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require "store.rkt")
(require "parser.rkt")
(require "ast-mapper.rkt")
(require (only-in (lib "eopl.ss" "eopl") eopl:error cases))

(initialize-store!)
(initialize-env!)

(define (make-thunk thunk-proc)
  (thunk-val thunk-proc (box #f)))

(define (type-error expected actual line)
  (eopl:error 'type-error "Line ~a: Expected ~a, got ~a" line expected actual))

(define (check-type val expected-type line)
  (cases type expected-type
    (int-type () (unless (num-val? val) (type-error "int" (expval->type-name val) line)))
    (float-type () (unless (num-val? val) (type-error "float" (expval->type-name val) line)))
    (string-type () (unless (string-val? val) (type-error "string" (expval->type-name val) line)))
    (bool-type () (unless (bool-val? val) (type-error "bool" (expval->type-name val) line)))
    (list-type (elem-type)
      (unless (list-val? val) (type-error "list" (expval->type-name val) line))
      (for-each (lambda (elem) (check-type elem elem-type line))
                (expval->list val)))
    (else #t)))

(define (expval->type-name val)
  (cases expval val
    (num-val (n) "number")
    (float-val (f) "float")
    (bool-val (b) "boolean")
    (string-val (s) "string")
    (list-val (l) "list")
    (null-val () "null")
    (void-val () "void")
    (closure-val (p b e) "function")
    (thunk-val (t m) "thunk")))

(define (types-compatible? t1 t2)
  (or (equal? t1 t2)
      (and (int-type? t1) (float-type? t2))
      (and (float-type? t1) (int-type? t2))))

(define (infer-type val)
  (cases expval val
    (num-val (n) (int-type))
    (float-val (f) (float-type))
    (bool-val (b) (bool-type))
    (string-val (s) (string-type))
    (list-val (vals)
      (if (null? vals)
          (list-type (int-type))
          (list-type (infer-type (car vals)))))
    (null-val () (null-type))
    (void-val () (void-type))
    (else (eopl:error 'type-inference "Cannot infer type from value ~a" val))))

(define (eval-expression expr env)
  (cases expression expr
    (const-exp (val) val)
    (var-exp (var)
      (let ((ref (apply-env var env)))
        (if ref
            (force-expval (deref ref))
            (eopl:error 'unbound-variable "Variable ~a not defined" var))))
    (len-exp (expr)
      (let ((val (force-expval (eval-expression expr env))))
        (cond
          [(list-val? val) (num-val (length (expval->list val)))]
          [(string-val? val) (num-val (string-length (expval->string val)))]
          [else (eopl:error 'type-error "len() expects list or string, got ~a" (expval->type-name val))])))
    (index-exp (base idx-expr)
      (let ((base-val (force-expval (eval-expression base env))))
        (cond
          [(list-val? base-val)
           (let ((idx-val (force-expval (eval-expression idx-expr env))))
             (unless (num-val? idx-val)
               (eopl:error 'type-error "Index must be an integer"))
             (let* ((idx (expval->num idx-val))
                    (elems (expval->list base-val)))
               (unless (and (integer? idx) (>= idx 0) (< idx (length elems)))
                 (eopl:error 'index-out-of-range "Index ~a out of range (length ~a)" idx (length elems)))
               (list-ref elems idx)))]
          [(string-val? base-val)
           (let ((idx-val (force-expval (eval-expression idx-expr env))))
             (unless (num-val? idx-val)
               (eopl:error 'type-error "Index must be an integer"))
             (let* ((idx (expval->num idx-val))
                    (str (expval->string base-val))
                    (len (string-length str)))
               (unless (and (integer? idx) (>= idx 0) (< idx len))
                 (eopl:error 'index-out-of-range "Index ~a out of range (length ~a)" idx len))
               (string-val (string (string-ref str idx)))))]
          [else (eopl:error 'type-error "Attempt to index non-indexable type (~a). Only list and string are indexable." (expval->type-name base-val))])))
    (binary-op-exp (op left right)
      (let ((left-val (eval-expression left env))
            (right-val (eval-expression right env)))
        (apply-binary-op op left-val right-val)))
    (unary-op-exp (op e)
      (let ((val (eval-expression e env)))
        (apply-unary-op op val)))
    (call-exp (func-name args)
      (let ((func-val (eval-expression (var-exp func-name) env)))
        (cases expval func-val
          (closure-val (params body closure-env)
            (if (= (length params) (length args))
                (let ((arg-thunks
                        (map (lambda (arg)
                               (make-thunk (lambda () (eval-expression arg env))))
                             args)))
                  (eval-function-call params arg-thunks body closure-env))
                (eopl:error 'arity-mismatch "Function ~a expects ~a arguments, got ~a" func-name (length params) (length args))))
          (else (eopl:error 'not-a-function "~a is not a function" func-name)))))
    (list-exp (elements)
      (list-val (map (lambda (elem) (eval-expression elem env)) elements)))
    (null-exp () (null-val))))

(define in-loop? #f)
(define break-flag #f)
(define continue-flag #f)

(define (enter-loop) (set! in-loop? #t))
(define (exit-loop) (set! in-loop? #f))
(define (signal-break) (set! break-flag #t))
(define (signal-continue) (set! continue-flag #t))
(define (clear-flags)
  (set! break-flag #f)
  (set! continue-flag #f))

(define (apply-binary-op op left-val right-val)
  (let ((left-val (force-expval left-val))
        (right-val (force-expval right-val)))
    (cond
      [(and (num-val? left-val) (num-val? right-val))
       (let ((l (expval->num left-val))
             (r (expval->num right-val)))
         (case op
           [(+) (num-val (+ l r))]
           [(-) (num-val (- l r))]
           [(*) (num-val (* l r))]
           [(/) (if (zero? r)
                    (eopl:error 'division-by-zero "Division by zero")
                    (float-val (/ (exact->inexact l) r)))]
           [(//) (if (zero? r)
                     (eopl:error 'division-by-zero "Division by zero")
                     (num-val (quotient l r)))]
           [(%) (num-val (remainder l r))]
           [(>) (bool-val (> l r))]
           [(<) (bool-val (< l r))]
           [(>=) (bool-val (>= l r))]
           [(<=) (bool-val (<= l r))]
           [(==) (bool-val (= l r))]
           [(!=) (bool-val (not (= l r)))]
           [else (eopl:error 'unknown-operator "Unknown numeric operator: ~a" op)]))]
      [(and (bool-val? left-val) (bool-val? right-val))
       (let ((l (expval->bool left-val))
             (r (expval->bool right-val)))
         (case op
           [(and) (bool-val (and l r))]
           [(or)  (bool-val (or l r))]
           [(==)  (bool-val (eq? l r))]
           [(!=)  (bool-val (not (= l r)))]
           [else (eopl:error 'unknown-operator "Unknown boolean operator: ~a" op)]))]
      [(and (string-val? left-val) (string-val? right-val))
       (let ((l (expval->string left-val))
             (r (expval->string right-val)))
         (case op
           [(==) (bool-val (string=? l r))]
           [(!=) (bool-val (not (string=? l r)))]
           [else (eopl:error 'unknown-operator "Unknown string operator: ~a" op)]))]
      [(and (list-val? left-val) (list-val? right-val))
       (let ((l (expval->list left-val))
             (r (expval->list right-val)))
         (case op
           [(==) (bool-val (equal? l r))]
           [(!=) (bool-val (not (equal? l r)))]
           [else (eopl:error 'unknown-operator "Unknown list operator: ~a" op)]))]
      [else (type-error "compatible types" (expval->type-name left-val) (expval->type-name right-val))])))

(define (apply-unary-op op val)
  (let ((val (force-expval val)))
    (cond
      [(num-val? val)
       (let ((n (expval->num val)))
         (case op
           ((neg) (num-val (- n)))
           [else (eopl:error 'unknown-operator "Unknown unary operator: ~a" op)]))]
      [(bool-val? val)
       (let ((b (expval->bool val)))
         (case op
           ((not) (bool-val (not b)))
           [else (eopl:error 'unknown-operator "Unknown unary operator: ~a" op)]))]
      [else (type-error "numeric or boolean" (expval->type-name val) #f)])))

(define (map-indexed proc lst)
  (let loop ((i 0) (lst lst) (acc '()))
    (if (null? lst)
        (reverse acc)
        (loop (add1 i) (cdr lst) (cons (proc i (car lst)) acc)))))

(define (old-char str idx)
  (string-val (string (string-ref str idx))))

(define (update-index-of-value base-val idx new-val)
  (cond
    [(list-val? base-val)
     (let* ((vals (expval->list base-val))
            (len (length vals)))
       (unless (and (integer? idx) (>= idx 0) (< idx len))
         (eopl:error 'index-out-of-range "Index ~a out of range (length ~a)" idx len))
       (list-val (map-indexed (lambda (i old) (if (= i idx) new-val old)) vals)))]
    [(string-val? base-val)
     (let* ((str (expval->string base-val))
            (len (string-length str)))
       (unless (and (integer? idx) (>= idx 0) (< idx len))
         (eopl:error 'index-out-of-range "Index ~a out of range (length ~a)" idx len))
       (let* ((replacement (expval->string new-val))
              (c (if (> (string-length replacement) 0)
                     (string-ref replacement 0)
                     (eopl:error 'type-error "Replacement string cannot be empty"))))
         (string-val
          (string-append
           (substring str 0 idx)
           (string c)
           (substring str (add1 idx))))))]
    [else
     (eopl:error 'type-error "Attempt to index-assign non-indexable type (~a)" (expval->type-name base-val))]))

(define (assign-index-rec lvalue-expr idx new-val env)
  (cases expression lvalue-expr
    (var-exp (var)
      (let ((ref (apply-env var env)))
        (unless ref (eopl:error 'unbound-variable "Variable ~a not defined" var))
        (let* ((old-val (force-expval (deref ref)))
               (updated-val (update-index-of-value old-val idx new-val)))
          (setref! ref updated-val)
          env)))
    (index-exp (base-expr inner-idx-expr)
      (let* ((inner-idx-val (eval-expression inner-idx-expr env))
             (i (expval->num inner-idx-val))
             (base-val (force-expval (eval-expression base-expr env))))
        (cond
          [(list-val? base-val)
           (let ((old-sub (list-ref (expval->list base-val) i)))
             (let ((new-sub (update-index-of-value old-sub idx new-val)))
               (assign-index-rec base-expr i new-sub env)))]
          [(string-val? base-val)
           (let ((str (expval->string base-val)))
             (let ((old-char (string-val (string (string-ref str i))))
                   (new-sub (update-index-of-value old-char idx new-val)))
               (assign-index-rec base-expr i new-sub env)))]
          [else (eopl:error 'type-error "Left-hand side is not indexable")])))
    (else (eopl:error 'type-error "Invalid assignment target; must be variable or index expression"))))

(define (eval-statement stmt env)
  (cases statement stmt
    (var-decl-stmt (var type expr)
      (let ((val (eval-expression expr env)))
        (let ((final-type (if (not type) (infer-type val) type)))
          (check-type val final-type #f)
          (extend-env var (newref val) env))))
    (func-decl-stmt (name params return-type body)
      (let ((placeholder (newref #f)))
        (set! env (extend-env name placeholder env))
        (let ((closure (closure-val params body env)))
          (setref! placeholder closure)
          env)))
    (assign-stmt (var expr)
      (let ((val (eval-expression expr env))
            (ref (apply-env var env)))
        (if ref
            (begin (setref! ref val) env)
            (eopl:error 'unbound-variable "Variable ~a not defined" var))))
    (index-assign-stmt (base idx expr)
      (let ((val (eval-expression expr env))
            (idx-val (eval-expression idx env)))
        (unless (num-val? idx-val)
          (eopl:error 'type-error "Index must be an integer"))
        (assign-index-rec base (expval->num idx-val) val env)))
    (break-stmt ()
      (if in-loop?
          (begin (signal-break) env)
          (eopl:error 'break "Break outside loop")))
    (continue-stmt ()
      (if in-loop?
          (begin (signal-continue) env)
          (eopl:error 'continue "Continue outside loop")))
    (if-stmt (condition then-branch else-branch)
      (let ((cond-val (eval-expression condition env)))
        (if (expval->bool cond-val)
            (eval-statement then-branch env)
            (if else-branch
                (eval-statement else-branch env)
                env))))
    (while-stmt (condition body)
      (enter-loop)
      (let loop ((cur-env env))
        (clear-flags)
        (let ((cond-val (eval-expression condition cur-env)))
          (when break-flag (exit-loop))
          (when (and (expval->bool cond-val) (not break-flag))
            (let ((new-env (eval-statement body cur-env)))
              (cond
                [break-flag (exit-loop) new-env]
                [continue-flag (loop new-env)]
                [else (loop new-env)])))
          (exit-loop)
          cur-env)))
    (for-stmt (init condition update body)
      (enter-loop)
      (let ((cur-env (eval-statement init env)))
        (let loop ((cur-env cur-env))
          (clear-flags)
          (let ((cond-val (eval-expression condition cur-env)))
            (if (and (expval->bool cond-val) (not break-flag))
                (let ((body-env (eval-statement body cur-env)))
                  (cond
                    [break-flag (exit-loop) body-env]
                    [continue-flag
                     (let ((update-env (eval-statement update body-env)))
                       (loop update-env))]
                    [else
                     (let ((update-env (eval-statement update body-env)))
                       (loop update-env))]))
                (begin (exit-loop) cur-env))))))
    (block-stmt (statements)
      (foldl (lambda (stmt cur-env) (eval-statement stmt cur-env))
             env
             statements))
    (call-stmt (func-name args)
      (eval-expression (call-exp func-name args) env)
      env)
    (print-stmt (exprs)
      (for-each
        (lambda (expr)
          (let ((val (eval-expression expr env)))
            (display (expval->string val))
            (display " ")))
        exprs)
      (newline)
      env)
    (return-stmt (expr)
      (if expr
          (eval-expression expr env)
          (void-val)))
    (empty-stmt () env)
    (else (eopl:error 'eval-statement "Unhandled statement type: ~a" stmt))))

(define (eval-function-call params arg-vals body closure-env)
  (let ((new-env closure-env))
    (for-each
      (lambda (p arg-thunk)
        (cases param p
          (a-param (name param-type)
            (let ((ref (newref arg-thunk)))
              (set! new-env (extend-env name ref new-env))))))
      params arg-vals)
    (eval-statement body new-env)))

(define (eval-program prog)
  (cases program prog
    (a-program (statements)
      (let ((env (get-env)))
        (for-each
          (lambda (stmt)
            (set! env (eval-statement stmt env))
            (gc! env))
          statements)
        (void-val)))))

(define (interpret source-code)
  (let* ((parsed (parse-full source-code))
         (typed-ast (list-ast->typed-ast parsed)))
    (eval-program typed-ast)))

(provide interpret eval-program eval-statement eval-expression)
