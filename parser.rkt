#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)
(require "lexer.rkt")

(define parse-full
  (parser
   (start Program)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value)
            (error (format "Parse error near token: ~a" tok-name))))
   (tokens value-tokens keyword-tokens symbol-tokens)
   (grammar
    [Program
     [(StatementList) (list 'Program $1)]]

    [StatementList
     [(Statement StatementList) (cons $1 $2)]
     [() '()]]

    [Statement
     [(OtherStatement) $1]
     [(IfStatement) $1]
     [(LoopStatement) $1]
     [(IndexAssignment) $1]]

    [OtherStatement
     [(VariableDeclaration) $1]
     [(FunctionDeclaration) $1]
     [(Assignment) $1]
     [(Block) $1]
     [(FunctionCallStmt) $1]
     [(PrintStatement) $1]
     [(ReturnStatement) $1]
     [(BreakStatement) $1]
     [(ContinueStatement) $1]]

    [BreakStatement
     [(BREAK SEMICOLON) (list 'Break)]]

    [ContinueStatement
     [(CONTINUE SEMICOLON) (list 'Continue)]]

    [Block
     [(OCB StatementList CCB) (list 'Block $2)]]

    [VariableDeclaration
     [(VAR ID COLON Type ASSIGNMENT Expression SEMICOLON)
      (list 'VarDecl $2 $4 $6)]
     [(VAR ID ASSIGNMENT Expression SEMICOLON)
      (list 'VarDeclInfer $2 $4)]]

    [IndexAssignment
     [(PostfixExpr ASSIGNMENT Expression SEMICOLON)
      (list 'IndexAssign $1 $3)]]

    [Type
     [(INTTYPE) "int"]
     [(FLOATTYPE) "float"]
     [(STRINGTYPE) "string"]
     [(BOOLTYPE) "bool"]
     [(LISTTYPE OB Type CB) (list 'list $3)]
     [(LISTTYPE LT Type GT) (list 'list $3)]
     [(NULLTYPE) "nulltype"]
     [(VOID) "void"]]

    [FunctionDeclaration
     [(FUNC ID OP ParameterList CP COLON Type Block)
      (list 'FuncDecl $2 $4 $7 $8)]]

    [ParameterList
     [(ParameterListNonEmpty) $1]
     [() '()]]

    [ParameterListNonEmpty
     [(Parameter) (list $1)]
     [(Parameter COMMA ParameterListNonEmpty) (cons $1 $3)]]

    [Parameter
     [(ID COLON Type) (list 'Param $1 $3)]]

    [Assignment
     [(ID ASSIGNMENT Expression SEMICOLON)
      (list 'Assign $1 $3)]]

    [IfStatement
     [(IF OP Expression CP Block ELSE Block)
      (list 'If $3 $5 $7)]
     [(IF OP Expression CP Block)
      (list 'If $3 $5 'None)]]

    [LoopStatement
     [(WhileLoop) $1]
     [(ForLoop) $1]]

    [WhileLoop
     [(WHILE OP Expression CP Block)
      (list 'While $3 $5)]]

    [ForLoop
     [(FOR OP ForAssignment SEMICOLON Expression SEMICOLON ForAssignment CP Block)
      (list 'For $3 $5 $7 $9)]]

    [ForAssignment
     [(ID ASSIGNMENT Expression) (list 'Assign $1 $3)]
     [(VAR ID COLON Type ASSIGNMENT Expression) (list 'VarDecl $2 $4 $6)]]

    [FunctionCallExpr
     [(ID OP ArgumentList CP)
      (list 'Call $1 $3)]
     [(LEN OP Expression CP) (list 'Len $3)]]

    [FunctionCallStmt
     [(FunctionCallExpr SEMICOLON) $1]]

    [ArgumentList
     [(ArgumentListNonEmpty) $1]
     [() '()]]

    [ArgumentListNonEmpty
     [(Expression) (list $1)]
     [(Expression COMMA ArgumentListNonEmpty) (cons $1 $3)]]

    [PrintStatement
     [(PRINT OP ExpressionList CP SEMICOLON)
      (list 'Print $3)]]

    [ReturnStatement
     [(RETURN Expression SEMICOLON) (list 'Return $2)]
     [(RETURN SEMICOLON) (list 'Return 'None)]]

    [Expression
     [(LogicalExpression) $1]]

    [LogicalExpression
     [(LogicalExpression OR LogicalTerm) (list 'or $1 $3)]
     [(LogicalTerm) $1]]

    [LogicalTerm
     [(LogicalTerm AND ComparativeExpression) (list 'and $1 $3)]
     [(ComparativeExpression) $1]
     [(NOT LogicalTerm) (list 'not $2)]]

    [ComparativeExpression
     [(ArithmeticExpression ComparativeOperator ArithmeticExpression) (list $2 $1 $3)]
     [(ArithmeticExpression) $1]]

    [ArithmeticExpression
     [(ArithmeticExpression AdditiveOperator Term) (list $2 $1 $3)]
     [(Term) $1]]

    [Term
     [(Term MultiplicativeOperator Power) (list $2 $1 $3)]
     [(Power) $1]]

    [Power
     [(Factor POWER Power) (list '^ $1 $3)]
     [(Factor) $1]]

    [PostfixExpr
     [(Primary) $1]
     [(PostfixExpr OB Expression CB) (list 'Index $1 $3)]]

    [Primary
     [(ID) $1]
     [(Literal) $1]
     [(FunctionCallExpr) $1]
     [(OP Expression CP) $2]
     [(NULL) 'NULL]]

    [Factor
     [(MINUS Factor) (list 'neg $2)]
     [(PostfixExpr) $1]]

    [Literal
     [(INT) $1]
     [(FLOAT) $1]
     [(STRING) $1]
     [(ListLiteral) $1]
     [(TRUE) 'true]
     [(FALSE) 'false]]

    [ListLiteral
     [(OB ExpressionList CB) (list 'List $2)]]

    [ExpressionList
     [(ExpressionListNonEmpty) $1]
     [() '()]]

    [ExpressionListNonEmpty
     [(Expression) (list $1)]
     [(Expression COMMA ExpressionListNonEmpty) (cons $1 $3)]]

    [ComparativeOperator
     [(LT) '<]
     [(GT) '>]
     [(LET) '<=]
     [(GET) '>=]
     [(EQUALS) '==]
     [(NOTEQUALS) '!=]]

    [LogicalOperator
     [(AND) 'and]
     [(OR) 'or]]

    [AdditiveOperator
     [(PLUS) '+]
     [(MINUS) '-]]

    [MultiplicativeOperator
     [(TIMES) '*]
     [(DIVIDE) '/]
     [(MODULO) '%]
     [(INT-DIVIDE) '//]]

    [UnaryOperator
     [(MINUS) 'neg]
     [(NOT) 'not]])))
(provide parse-full)
