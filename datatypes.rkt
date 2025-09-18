#lang racket
(require (lib "eopl.ss" "eopl"))

(define (report-expval-extractor-error! type)
  (eopl:error 'invalid-value "invalid value cast to ~s" type))

(define reference?
  (lambda (var)
    (integer? var)))

(define-datatype environment environment?
  (empty-environment)
  (extend-environment (var string?)
                      (val reference?)
                      (env environment?)))

(define-datatype expval expval?
  (num-val (num number?))
  (float-val (float number?))
  (bool-val (bool boolean?))
  (string-val (string string?))
  (list-val (vals (list-of expval?)))
  (null-val)
  (void-val)
  (thunk-val (thunk procedure?) (memo box?))
  (closure-val (params (list-of param?))
               (body statement?)
               (env environment?)))

(define (force-expval v)
  (cases expval v
    (thunk-val (thunk memo)
      (let ((cached (unbox memo)))
        (if cached
            cached
            (let ((res (thunk)))
              (set-box! memo res)
              res))))
    (else v)))

(define expval->num
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (num-val (num) num)
        (else (report-expval-extractor-error! "number"))))))

(define expval->bool
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (bool-val (bool) bool)
        (else (report-expval-extractor-error! "boolean"))))))

(define expval->string
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (num-val (n) (number->string n))
        (float-val (f) (number->string f))
        (bool-val (b) (if b "true" "false"))
        (string-val (s) s)
        (list-val (l) (format "[~a]" (string-join (map expval->string l) ", ")))
        (null-val () "NULL")
        (void-val () "void")
        (closure-val (p b e) "<function>")
        (else (report-expval-extractor-error! "string"))))))

(define expval->list
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (list-val (vals) vals)
        (else (report-expval-extractor-error! "list"))))))

(define expval->void
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (void-val () 'void)
        (else (report-expval-extractor-error! "void"))))))

(define num-val?
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (num-val (num) #t)
        (else #f)))))

(define bool-val?
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (bool-val (bool) #t)
        (else #f)))))

(define string-val?
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (string-val (string) #t)
        (else #f)))))

(define list-val?
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (list-val (vals) #t)
        (else #f)))))

(define void-val?
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (void-val () #t)
        (else #f)))))

(define-datatype program program?
  (a-program (statements (list-of statement?))))

(define-datatype statement statement?
  (var-decl-stmt (var string?) (type (maybe type?)) (expr expression?))
  (func-decl-stmt (name string?) (params (list-of param?)) (return-type type?) (body statement?))
  (assign-stmt (var string?) (expr expression?))
  (index-assign-stmt (base expression?) (index expression?) (expr expression?))
  (if-stmt (condition expression?) (then-branch statement?) (else-branch (maybe statement?)))
  (while-stmt (condition expression?) (body statement?))
  (for-stmt (init statement?) (condition expression?) (update statement?) (body statement?))
  (block-stmt (statements (list-of statement?)))
  (call-stmt (func-name string?) (args (list-of expression?)))
  (print-stmt (exprs (list-of expression?)))
  (return-stmt (expr (maybe expression?)))
  (break-stmt)
  (continue-stmt)
  (empty-stmt))

(define-datatype expression expression?
  (const-exp (val expval?))
  (var-exp (var string?))
  (index-exp (base expression?) (index expression?))
  (call-exp (func-name string?) (args (list-of expression?)))
  (unary-op-exp (op symbol?) (expr expression?))
  (binary-op-exp (op symbol?) (left expression?) (right expression?))
  (list-exp (elements (list-of expression?)))
  (len-exp (expr expression?))
  (null-exp))

(define-datatype type type?
  (int-type)
  (float-type)
  (string-type)
  (bool-type)
  (list-type (element-type type?))
  (null-type)
  (void-type))

(define-datatype param param?
  (a-param (name string?) (param-type type?)))

(define (int-type? t)
  (cases type t
    (int-type () #t)
    (else #f)))

(define (float-type? t)
  (cases type t
    (float-type () #t)
    (else #f)))

(define (string-type? t)
  (cases type t
    (string-type () #t)
    (else #f)))

(define (bool-type? t)
  (cases type t
    (bool-type () #t)
    (else #f)))

(define (list-type? t)
  (cases type t
    (list-type (elem) #t)
    (else #f)))

(define (null-type? t)
  (cases type t
    (null-type () #t)
    (else #f)))

(define (void-type? t)
  (cases type t
    (void-type () #t)
    (else #f)))

(define float-val?
  (lambda (val)
    (let ((v (force-expval val)))
      (cases expval v
        (float-val (float) #t)
        (else #f)))))

(provide (all-defined-out))
