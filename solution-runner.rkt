#lang racket
(require "lexer.rkt")
(require "parser.rkt")
(require "ast-mapper.rkt")
(require "interpreter.rkt")

(define test-file "solutions/solution1.txt")

(define test-source
  (file->string test-file))

;; lexing
(define tokens (let loop ((res '()) (port (open-input-string test-source)))
                 (let ((tok (lexer-full port)))
                   (if (eq? tok 'EOF)
                       (reverse res)
                       (loop (cons tok res) port)))))

;; parser
(define (make-token-getter input-str)
  (define in-port (open-input-string input-str))
  (lambda () (lexer-full in-port)))

(define token-getter (make-token-getter test-source))
(define parsed (parse-full token-getter))

;; ast-mapper
(define typed-ast (list-ast->typed-ast parsed))

;; run
(displayln "Program Output:")
(eval-program typed-ast)
