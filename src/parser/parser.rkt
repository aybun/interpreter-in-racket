#lang racket

(require dyoo-while-loop)
(require (prefix-in token. "../token/token.rkt"))
(require (prefix-in lexer. "../lexer/lexer.rkt"))
(require (prefix-in ast.   "../ast/ast.rkt"))

;; (define _ 0)
(define LOWEST 1)
(define EQUALS 2)       ; ==
(define LESSGREATER 3)  ; > or <
(define SUM 4)          ; +
(define PRODUCT 5)      ; *
(define PREFIX 6)       ; -X or !X
(define CALL 7)         ; myFunction(X)



(define precedences (hash
    token.EQ       EQUALS
    token.NOT_EQ   EQUALS
    token.LT       LESSGREATER
    token.GT       LESSGREATER
    token.PLUS     SUM
    token.MINUS    SUM
    token.SLASH    PRODUCT
    token.ASTERISK PRODUCT
    token.LPAREN   CALL
                         ))
(define Parser
  (class object%
    (field errors curToken peekToken prefixParseFns infixParseFns)
    (init-field l)
    (super-new)
    (define/private (initialize)
      (begin

        (set! erorrs '())
        (set! curToken null)
        (set! peekToken null)
        (set! prefixParseFns (hash
                                token.IDENT    parseIdentifier
                                token.INT      parseIntegerLiteral
                                token.BANG     parsePrefixExpression
                                token.MINUS    parsePrefixExpression
                                token.TRUE     parseBoolean
                                token.FALSE    parseBoolean
                                token.LPAREN   parseGroupedExpression
                                token.IF       parseIfExpression
                                token.FUNCTION parseFunctionLiteral
                                ))

        (set! infixParseFns (hash
                                token.PLUS    parseInfixExpression
                                token.MINUS      parseInfixExpression
                                token.SLASH     parseInfixExpression
                                token.ASTERISK    parseInfixExpression
                                token.EQ    parseInfixExpression
                                token.NOT_EQ    parseInfixExpression
                                token.LT    parseInfixExpression
                                token.GT    parseInfixExpression


                                token.LPAREN    parseCallExpression


                                ))
        ;// Read two tokens, so curToken and peekToken are both set
        (nextToken)
        (nextToken)


       );::END begin
    );  ::END initialize
    (initialize);; Might need to call at the bottom.

    (define/private (nextToken) (begin
                                  (set! curToken peekToken)
                                  (set! peekToken (send l NextToken))))

    (define/private (curTokenIs t) (equal? (get-field Type curToken ) t))
    (define/private (expectPeek t) (if (peekTokenIs t)
                                       (begin  (nextToken)   #t)
                                       (begin  (peekError t) #f)
                                    ))



    (define/public (Errors) errors)
    (define/private (peekError t)
      (define msg (format "expected next token to be ~a, got ~a instead" t (get-field Type peekToken)))
      (append errors (list msg))
    )

    (define/private (noPrefixParseFnError t)
      (define msg (format "no prefix parse function for ~a found" t ))
      (append errors (list msg))
    )

    (define/public (ParseProgram)
      (define program (new ast.Program [Statements (list)] ))
      (while (not (curTokenIs token.EOF)) (begin
                                            (define stmt (parseStatement))
                                            (define statements (get-field Statements program))
                                            (unless (null? stmt) (set-field! Statements program (append statements (list stmt))))
                                            (nextToken)
                                            ))
      program
    )

    (define/private (parseStatement) (case (get-field Type curToken)
                                       [(token.LET)    (parseLetStatement)]
                                       [(token.RETURN) (parseReturnStatement)]
                                       [ else          (parseExpressionStatement)]
                                      )
    )

    (define/private (parseLetStatement)
      ;; This is ugly. I may comeback to re-write this.
      (define token curToken)
      (if (not (expectPeek token.IDENT))

          null

         (begin
           (define name (new ast.Identifier [Token curToken] [Value (get-field Literal curToken)] ))
           (if (not (expectPeek token.ASSIGN))

               null

               (begin ;;again
                 (nextToken)
                 (define value (parseExpression LOWEST))
                 (when (peekTokenIs token.SEMICOLO) (nextToken))

                 (define stmt (new ast.LetStatement [Token token] [Name name] [Value value]))
                 stmt

               );; END begin again

           )
         );;END begin
      )
    )

    (define/private (parseReturnStatement)
      (define token curToken)
      (nextToken)
      (define returnValue (parseExpression LOWEST))
      (when (peekTokenIs token.SEMICOLON) (nextToken))

      (define stmt (new ast.ReturnStatement [Token token] [ReturnValue returnValue]))
      stmt

    )

    (define/private (parseExpressionStatement)
      (define token curToken)
      (define expression (parseExpression LOWEST))
      (when (peekTokenIs token.SEMICOLON) (nextToken))

      (define stmt (new ast.ExpressionStatement [Token token ] [Expression expression]))
      stmt

    )

    (define/private (parseExpression precedence)
      (define prefix (hash-ref prefixParseFns (get-field Type curToken) null))
      (define returnValue null)
      (if (null? prefix)

          (begin; ::THEN
            (noPrefixParsFnError (get-field Type curToken))
            (set! returnValue null)
          )

          (begin; ::ELSE

            (define leftExp (prefix))
            (while (and
                     (not (peekTokenIs token.SEMICOLON))
                     (< precedence peekPrecedence)
                   )

                   (begin

                     (define infix (hash-ref infixParseFns (get-field Type curToken) null))
                     (when (null? infix) (set! returnValue leftExp) (break))
                     (nextToken)
                     (set! leftExp (infix leftExp))
                   )
               
            );; END WHILE
            (set! returnValue leftExp)
          ); ::END ELSE
      )

     ; :: RETURN ::
      returnValue

    )  ; :: END parseExpression

    (define/private (peekPrecedence)
      (define p (hash-ref precedences (get-field Type peekToken) null))
      (if (null? p) LOWEST p)
    )

    (define/private (curPrecedence)
      ;; Alternative way to write.
      (hash-ref precedences (get-field Type curToken) LOWEST)
    )

    (define/private (parseIdentifier)
      (new ast.Identifier [Token curToken] [Value (get-field Literal curToken)])
    )

    (define/private (parseIntegerLiteral)
      (define token curToken)
      (define value (string->number (get-field Literal curToken)))
      (if (integer? value)
          (new ast.IntegerLiteral [Token token] [Value value])

          (begin
            (define msg (format "could not parse ~a as integer" (get-field Literal curToken)))
            (set! errors (append errors (list msg)))
            null
          )
      )
    )

    (define/private (parsePrefixExpression)
      (define token curToken)
      (define operator (get-field Literal curToken))

      (nextToken)

      (define right (parseExpression PREFIX))

      (new ast.PrefixExpression [Token token] [Operator operator] [Rigt right])

    )

    (define/private (parseInfixexpression left)
      (define token curToken)
      (define operator (get-field Literal curToken))

      (define precedence (curPrecedence))
      (nextToken)
      (define right (parseExpression precedence))

      (new ast.InfixExpression [Token token] [Operator operator] [Left left] [Right right])
    )

    (define/private (parseBoolean)
      (new ast.Boolean [Token curToken] [Value (curTokenIs token.TRUE)])
    )

    (define/private (parseGroupedExpression)
      (nextToken)
      (define exp (parseExpression LOWEST))
      (if (not (expectPeek token.RPAREN)) null exp)
    )



    (define/private (parseIfExpression)
      (define returnValue null)
      (while #t (begin

                 (define token curToken)

                 (unless (expectPeek token.LPAREN) (set! returnValue null) (break))

                 (nextToken)

                 (define condition (parseExpression LOWEST))

                 (unless (expectPeek token.RPAREN) (set! returnValue null) (break))

                 (unless (expectPeek token.LBRACE) (set! returnValue null) (break))

                 (define consequence (parseBlockStatement))

                 (define alternative null)

                 (when (peekTokenIs token.ELSE)
                    (nextToken)
                    (unless (expectPeek token.LBRACE) (set! returnValue null) (break) )
                    (set! alternative (parseBlockStatement))
                 )

                 (define expression (new ast.IfExpression
                                         [Token token] [Condition condition]
                                         [Consequence consequence] [Alternative alternative]))

                 (set! returnValue expression)


                 (break); ::END while
                 ))

      returnValue

    ); ::END parseIfExpression

    (define/private (parseBlockStatement)
      (define token curToken)
      (define statements '())
      (nextToken)

      (while (and (not (curTokenIs token.RBRACE))
                  (not (curTokenIs token.EOF))
             )

             (begin
               (define stmt (parseStatement))
               (unless (null? stmt) (set! statements (append statements (list stmt))))
               (nextToken)

             )
      )
      
      (new ast.BlockStatement [Token token] [Statements statements])
    )

    (define/private (parseFunctionLiteral)
      (define returnValue null)

      (while #t (begin
                  (define token curToken)
                  (unless (expectPeek token.LPAREN) (set! returnValue null) (break))
                  (define parameters (parseFunctionParameters))
                  (unless (expectPeek token.LBRACE) (set! returnValue null) (break))
                  (define body (parseBlockStatement))
                  (set! returnValue (new ast.FunctionLiteral
                                         [Token token] [Parameters parameters ] [Body body] ))

                 (break)
                 ))

      returnValue

    )

    (define/private (parseFunctionParameters)
      (define returnValue null)


      (while #t (begin
                  (define identifiers (list))

                  (when (peekTokenIs token.RPAREN) (nextToken) (set! returnValue identifiers))

                  (nextToken)

                  (define ident (new ast.Identifier [Token curToken] [Value (get-field Literal curToken)]))
                  (set! identifiers (append identifiers (list ident)))

                  (while (peekTokenIs token.COMMA) (begin
                                                     (nextToken)
                                                     (nextToken)
                                                     (set! ident (new ast.Identifer [Token curToken] [Value (get-field Literal curToken)]))
                                                     (set! identifiers (append identifiers (list iden)))
                                                     ))
                  (unless (expectPeek token.RPAREN) (set! returnValue null) (break))

                  (set! returnValue identifiers)
                  (break)

                 ))
      
      returnValue

    ); :: parseFunctionParamenters

    (define/private (parseCallExpression func)
      (new ast.CallExpression [Token curToken] [Function func] [Arguments (parseCallArguments)])
    )

    (define/private (parseCallArguments)
      (define returnValue null)
      (while #t (begin
                  (define args (list))

                  (when (peekTokenIs token.RPAREN) (nextToken) (set! returnValue args) (break))

                  (nextToken)

                  (set! args (append args (list (parseExpression LOWEST))))

                  (while (peekTokenIs token.COMMA) (begin
                                                     (nextToken)
                                                     (nextToken)
                                                     (set! args (append args (list (parseExpression LOWEST))))

                                                     ))

                  (unless (expectPeek token.RPAREN) (set! returnValue null) (break))

                  (set! returnValue args)

                  (break)
                 ))
      returnValue
    ); :: END parseCallArguments

    ; ::Might not be needed.
    (define/private (registerPrefix tokenType fn) (hash-set! prefixParseFns tokenType fn))

    ; ::Might not be needed.
    (define/private (registerInfix tokenType fn) (hash-set! infixfixParseFns tokenType fn))


  );; END class
)

(define (New l) (new Parser [l l]))

