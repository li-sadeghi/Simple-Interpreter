#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens
  (INT FLOAT STRING ID TRUE FALSE NULL EOF))

(define-empty-tokens keyword-tokens
  (VAR FUNC IF ELSE WHILE FOR PRINT RETURN BREAK CONTINUE LEN
   INTTYPE FLOATTYPE STRINGTYPE BOOLTYPE LISTTYPE NULLTYPE VOID))

(define-empty-tokens symbol-tokens
  (SEMICOLON COLON COMMA OP CP OB CB OCB CCB
   ASSIGNMENT PLUS MINUS TIMES DIVIDE INT-DIVIDE MODULO POWER
   LT GT LET GET EQUALS NOTEQUALS AND OR NOT))

(define line-number 1)

(define (update-line-number lexeme)
  (for-each (λ (c) (when (char=? c #\newline)
                     (set! line-number (+ line-number 1))))
            (string->list lexeme)))

(define lexer-full
  (lexer
    ((:+ (:or #\space #\tab #\newline))
     (begin
       (update-line-number lexeme)
       (lexer-full input-port)))

    ((:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))
     (token-FLOAT (string->number lexeme)))

    ((:+ (char-range #\0 #\9))
     (token-INT (string->number lexeme)))

    ((:: #\" (:* (:or (:~ #\" #\\) 
                      (:: #\\ any-char))) 
         #\")
     (token-STRING
       (let* ([content (substring lexeme 1 (sub1 (string-length lexeme)))]
              [processed (regexp-replace* #px"\\\\n" 
                                          (regexp-replace* #px"\\\\\"" 
                                                           (regexp-replace* #px"\\\\\\\\"
                                                                            content "\\\\")
                                                           "\"")
                                          "\n")])
         processed)))

    ("var" (token-VAR)) ("func" (token-FUNC)) ("if" (token-IF)) ("else" (token-ELSE))
    ("while" (token-WHILE)) ("for" (token-FOR)) ("print" (token-PRINT))
    ("return" (token-RETURN)) ("break" (token-BREAK)) ("continue" (token-CONTINUE))
    ("len" (token-LEN))

    ("int" (token-INTTYPE)) 
    ("float" (token-FLOATTYPE))
    ("string" (token-STRINGTYPE)) 
    ("bool" (token-BOOLTYPE)) 
    ("list" (token-LISTTYPE)) 
    ("nulltype" (token-NULLTYPE))
    ("void" (token-VOID))

    ("true" (token-TRUE #t))
    ("false" (token-FALSE #f))
    ("NULL" (token-NULL 'NULL))

    ("=" (token-ASSIGNMENT)) ("+" (token-PLUS)) ("-" (token-MINUS))
    ("*" (token-TIMES)) ("/" (token-DIVIDE)) ("%" (token-MODULO)) ("^" (token-POWER))
    ("<=" (token-LET)) (">=" (token-GET)) ("==" (token-EQUALS)) ("!=" (token-NOTEQUALS))
    ("<" (token-LT)) (">" (token-GT)) ("//" (token-INT-DIVIDE))
    ("&&" (token-AND)) ("||" (token-OR)) ("and" (token-AND)) ("or" (token-OR))
    ("!" (token-NOT)) ("not" (token-NOT))

    (";" (token-SEMICOLON)) (":" (token-COLON)) ("," (token-COMMA))
    ("(" (token-OP)) (")" (token-CP)) ("[" (token-OB)) ("]" (token-CB))
    ("{" (token-OCB)) ("}" (token-CCB))

    ((:: (:or (char-range #\a #\z) (char-range #\A #\Z) #\_)
         (:* (:or (char-range #\a #\z) (char-range #\A #\Z)
                  (char-range #\0 #\9) #\_)))
     (token-ID (string->symbol lexeme)))
  
    ((eof) 'EOF)

    (any-char
     (lambda (lexeme)
       (error (format "Unexpected character '~a' at line ~a"
                      lexeme line-number))))))
(provide (all-defined-out))
