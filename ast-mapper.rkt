#lang racket

(require "datatypes.rkt")

(define (list->list-of-statement lst)
  (apply list lst))

(define (ast->stmt ast)
  (match ast
    [(list 'Program stmts)
     (a-program (list->list-of-statement (map ast->stmt stmts)))]
    [(list 'VarDecl id type expr)
     (var-decl-stmt (symbol->string id) (convert-type type) (ast->expr expr))]
    [(list 'VarDeclInfer id expr)
     (var-decl-stmt (symbol->string id) #f (ast->expr expr))]
    [(list 'Assign id expr)
     (assign-stmt (symbol->string id) (ast->expr expr))]
    [(list 'If cond then else)
     (if-stmt (ast->expr cond)
              (ast->stmt then)
              (if (or (eq? else #f) (eq? else 'None))
                  #f
                  (ast->stmt else)))]
    [(list 'While cond body)
     (while-stmt (ast->expr cond)
                 (ast->stmt body))]
    [(list 'For init cond update body)
     (for-stmt (ast->stmt init)
               (ast->expr cond)
               (ast->stmt update)
               (ast->stmt body))]
    [(list 'Block stmts)
     (block-stmt (list->list-of-statement (map ast->stmt stmts)))]
    [(list 'Print exprs)
     (print-stmt (map ast->expr exprs))]
    [(list 'Return expr)
     (return-stmt (if (or (eq? expr #f) (eq? expr 'None))
                      #f
                      (ast->expr expr)))]
    [(list 'Break) (break-stmt)]
    [(list 'Continue) (continue-stmt)]
    [(list 'FuncDecl name params ret-type body)
     (func-decl-stmt (symbol->string name) (map ast->stmt params)
                     (convert-type ret-type)
                     (ast->stmt body))]
    [(list 'Call fname args)
     (call-stmt (symbol->string fname) (map ast->expr args))]
    [(list 'Param name type)
     (a-param (symbol->string name) (convert-type type))]
    [(list 'IndexAssign target expr)
     (cond
       [(and (list? target) (equal? (car target) 'Index))
        (let ([base (cadr target)]
              [idx (caddr target)])
          (index-assign-stmt (ast->expr base)
                             (ast->expr idx)
                             (ast->expr expr)))]
       [(symbol? target)
        (assign-stmt (symbol->string target)
                     (ast->expr expr))]
       [else (error "Invalid assignment target in AST" target)])]
    [else (error "AST Mapper (stmt): unrecognized form" ast)]))

(define (ast->expr ast)
  (match ast
    [(list 'Call fname args)
     (call-exp (symbol->string fname) (map ast->expr args))]
    [(list 'Index base idx)
     (index-exp (ast->expr base) (ast->expr idx))]
    [(list 'Len expr)
     (len-exp (ast->expr expr))]
    [(? number? n) (const-exp (num-val n))]
    ['true (const-exp (bool-val #t))]
    ['false (const-exp (bool-val #f))]
    ['NULL (null-exp)]
    [(list 'List exprs) (list-exp (map ast->expr exprs))]
    [(? symbol? id) (var-exp (symbol->string id))]
    [(? string? s)
     (const-exp (string-val s))]
    [(list op l r)
     (cond
       [(member op '(+ - * / % > < >= <= == != and or //))
        (binary-op-exp op (ast->expr l) (ast->expr r))]
       [else (error "AST Mapper (expr): unrecognized binary operator" op)])]
    [(list op e)
     (cond
       [(member op '(neg not))
        (unary-op-exp op (ast->expr e))]
       [else (error "AST Mapper (expr): unrecognized unary operator" op)])]
    [(list 'BinaryOp '== l r)
     (binary-op-exp '== (ast->expr l) (ast->expr r))]
    [(list 'BinaryOp '!= l r)
     (binary-op-exp '!= (ast->expr l) (ast->expr r))]
    [(list 'BinaryOp '< l r)
     (binary-op-exp '< (ast->expr l) (ast->expr r))]
    [(list 'BinaryOp '>= l r)
     (binary-op-exp '>= (ast->expr l) (ast->expr r))]
    [else (error "AST Mapper (expr): unrecognized form" ast)]))

(define (list-ast->typed-ast ast)
  (ast->stmt ast))

(define (convert-type t)
  (match t
    ["int" (int-type)]
    ["float" (float-type)]
    ["string" (string-type)]
    ["bool" (bool-type)]
    ["nulltype" (null-type)]
    ["void" (void-type)]
    [(list 'list inner) (list-type (convert-type inner))]
    [else (error "Unknown type" t)]))

(provide list-ast->typed-ast)
