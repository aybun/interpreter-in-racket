#lang racket

(require rackunit)
(require (prefix-in token. "../token/token.rkt"))
(require (prefix-in lexer. "../lexer/lexer.rkt"))
(require (prefix-in ast.   "../ast/ast.rkt"))
(require (prefix-in parser. "parser.rkt"))


;; TODO
;; Jumping between zones defined in the code base
;; Code ToC similar to book ToC
;; Fast Code Navigation


(define-syntax-rule (get obj field)
  (get-field field obj)
)
;; Rewrite when ready.
(define-syntax-rule (get-nested-2 obj f1 f2)
  (get-field f2 (get-field f1 obj))
)


;; (define-syntax-rule (:: str) Define Comment Symbol
;; )

(define (TestLetStatements)
  (define tests (list
      (list "let x = 5;" "x" 5)
      (list "let y = true;" "y" #t)
      (list "let foobar = y;" "foobar" "y")
    )
  )

  (for/list ([tt tests]
             [i (in-naturals)]) ;0, 1, 2, ...

            (begin
              (define input              (first  tt))
              (define expectedIdentifier (second tt))
              (define expectedValue      (third  tt))


              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))

              (checkParserErrors p)

              (define stmts (get program Statements))
              (check-equal? (length stmts) 1 "CHECK len(statements)")

              (define stmt (first stmts))
              (testLetStatement stmt      expectedIdentifier)
              (testLiteralExpression (get stmt Value) expectedValue)

            );; ::END BEGIN
  );; :: END FOR

) ;; END TestLetStatement


(define (TestReturnStatements)
  (define tests (list
                  (list "return 5;"  5 )
                  (list "return true;" true )
                  (list "return foobar;" "foobar")))

  (for/list ([tt tests ]
             [i (in-naturals)]) ; 0, 1, 2, ...
    (begin
              (define input              (first  tt))
              (define expectedValue      (second tt))


              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))
              (checkParserErrors p)


              (define stmts (get program Statements))
              (check-equal? (length stmts) 1 "CHECK len(statements)")

              (define stmt (first stmts))
              (check-equal? (is-a? stmt ast.ReturnStatement) true)
              (testLiteralExpression (get stmt ReturnValue) expectedValue)

    )

  ); :: END FOR

); :: END TestReutnrStatements

(define (TestIdentifierExpression)

  (define tests (list
                  (list "foobar;")))

  (for/list ([tt tests ]
             [i (in-naturals)]) ; 0, 1, 2, ...
    (begin
              (define input  (first  tt))

              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))
              (checkParserErrors p)


              (define stmts (get program Statements))
              (check-equal? (length stmts) 1 "CHECK len(statements)")

              (define stmt (first stmts))
              (check-equal? (is-a? stmt ast.ExpressionStatement) true)

              (define ident (get stmt Expression))
              (check-equal? (is-a? ident ast.Identifier) true)
              (check-equal? (get ident Value) "foobar")
              (check-equal? (send ident TokenLiteral) "foobar")
    )
  )
); :: END TestIdentifierExpression

(define (TestIntegerLiteralExpression)

  (define tests (list
                  (list "5;")))

  (for/list ([tt tests ]
             [i (in-naturals)]) ; 0, 1, 2, ...
    (begin
              (define input  (first  tt))

              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))
              (checkParserErrors p)


              (define stmts (get program Statements))
              (check-equal? (length stmts) 1 "CHECK len(statements)")

              (define stmt (first stmts))
              (check-equal? (is-a? stmt ast.ExpressionStatement) true)

              (define literal (get stmt Expression))
              (check-equal? (is-a? literal ast.IntegerLiteral) true)
              (check-equal? (get literal Value) 5)
              (check-equal? (send literal TokenLiteral) "5")
    )
  )
); :: END TestIntegerLiteralExpression


(define (TestParsingPrefixExpressions)

  (define tests   (list
                    (list "!5;" "!" 5)
                    (list "-15;" "-" 15)
                    (list "!foobar;" "!" "foobar")
                    (list "-foobar;" "-" "foobar")
                    (list "!true;" "!" true)
                    (list "!false;" "!" false)))


  (for/list ([tt tests ]
             [i (in-naturals)]) ; 0, 1, 2, ...
    (begin
              (define input  (first  tt))
              (define operator (second tt))
              (define value (third tt))


              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))
              (checkParserErrors p)


              (define stmts (get program Statements))
              (check-equal? (length stmts) 1 "CHECK len(statements)")

              (define stmt (first stmts))
              (check-equal? (is-a? stmt ast.ExpressionStatement) true)

              (define expr (get stmt Expression))
              (check-equal? (is-a? expr ast.PrefixExpression) true)
              (check-equal? (get expr Operator) operator)
              (printf "in TestParsingPrefixExpression here\n")
              (testLiteralExpression (get expr Right) value)
    )
  )
); :: END TestParsingPrefixExpressions

(define (TestParsingInfixExpressions)

  (define tests
            (list
              (list "5 + 5;"  5  "+"  5)
              (list "5 - 5;"  5  "-"  5)
              (list "5 * 5;"  5  "*"  5)
              (list "5 / 5;"  5  "/"  5)
              (list "5 > 5;"  5  ">"  5)
              (list "5 < 5;"  5  "<"  5)
              (list "5 == 5;"  5  "=="  5)
              (list "5 != 5;"  5  "!="  5)
              (list "foobar + barfoo;"  "foobar"  "+"  "barfoo")
              (list "foobar - barfoo;"  "foobar"  "-"  "barfoo")
              (list "foobar * barfoo;"  "foobar"  "*"  "barfoo")
              (list "foobar / barfoo;"  "foobar"  "/"  "barfoo")
              (list "foobar > barfoo;"  "foobar"  ">"  "barfoo")
              (list "foobar < barfoo;"  "foobar"  "<"  "barfoo")
              (list "foobar == barfoo;"  "foobar"  "=="  "barfoo")
              (list "foobar != barfoo;"  "foobar"  "!="  "barfoo")
              (list "true == true"  true  "=="  true)
              (list "true != false"  true  "!="  false)
              (list "false == false"  false  "=="  false))
  )


  (for/list ([tt tests ]
             [i (in-naturals)]) ; 0, 1, 2, ...
    (begin
              (define input  (first  tt))
              (define leftValue(second tt))
              (define operator (third tt))
              (define rightValue (fourth tt))

              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))
              (checkParserErrors p)

              (define stmts (get program Statements))
              (check-equal? (length stmts) 1 "CHECK len(statements)")

              (printf "in TestParseInfixExpressions:\n")
              (printf "i : ~a\n" i)
              (printf "input : ~a\n" input)

              (define stmt (first stmts))
              (check-equal? (is-a? stmt ast.ExpressionStatement) true)

              (testInfixExpression (get stmt Expression) leftValue operator rightValue)

    )
  )
); :: END TestParsingInfixExpressions


(define (TestOperatorPrecedenceParsing)
  (define tests (list
                      '(
                        "-a * b"
                        "((-a) * b)"
                        )
                      '(
                        "!-a"
                        "(!(-a))"
                        )
                      '(
                        "a + b + c"
                        "((a + b) + c)"
                        )
                      '(
                        "a + b - c"
                        "((a + b) - c)"
                        )
                      '(
                        "a * b * c"
                        "((a * b) * c)"
                        )
                      '(
                        "a * b / c"
                        "((a * b) / c)"
                        )
                      '(
                        "a + b / c"
                        "(a + (b / c))"
                        )
                      '(
                        "a + b * c + d / e - f"
                        "(((a + (b * c)) + (d / e)) - f)"
                        )
                      '(
                        "3 + 4; -5 * 5"
                        "(3 + 4)((-5) * 5)"
                        )
                      '(
                        "5 > 4 == 3 < 4"
                        "((5 > 4) == (3 < 4))"
                        )
                      '(
                        "5 < 4 != 3 > 4"
                        "((5 < 4) != (3 > 4))"
                        )
                      '(
                        "3 + 4 * 5 == 3 * 1 + 4 * 5"
                        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
                        )
                      '(
                        "true"
                        "true"
                        )
                      '(
                        "false"
                        "false"
                        )
                      '(
                        "3 > 5 == false"
                        "((3 > 5) == false)"
                        )
                      '(
                        "3 < 5 == true"
                        "((3 < 5) == true)"
                        )
                      '(
                        "1 + (2 + 3) + 4"
                        "((1 + (2 + 3)) + 4)"
                        )
                      '(
                        "(5 + 5) * 2"
                        "((5 + 5) * 2)"
                        )
                      '(
                        "2 / (5 + 5)"
                        "(2 / (5 + 5))"
                        )
                      '(
                        "(5 + 5) * 2 * (5 + 5)"
                        "(((5 + 5) * 2) * (5 + 5))"
                        )
                      '(
                        "-(5 + 5)"
                        "(-(5 + 5))"
                        )
                      '(
                        "!(true == true)"
                        "(!(true == true))"
                        )
                      '(
                        "a + add(b * c) + d"
                        "((a + add((b * c))) + d)"
                        )
                      '(
                        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"
                        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"
                        )
                      '(
                        "add(a + b + c * d / f + g)"
                        "add((((a + b) + ((c * d) / f)) + g))"
                        )
                 )); END tests

  (for/list ([tt tests]
             [i (in-naturals 1)]) ;1, 2, 3, ...
            (begin
              (define input  (first  tt))
              (define expected (second tt))

              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))
              (checkParserErrors p)

              (define actual (send program String))

              (check-equal? actual expected)

            )
  )
);; END TestOperatorPrecedenceParsing

(define (TestBooleanExpression)
  (define tests (list (list "true;" #t) (list "false;" #f)))

  (for/list ([tt tests]
             [i (in-naturals 1)]) ;1, 2, 3, ...
            (begin
              (define input  (first  tt))
              (define expectedBoolean (second tt))

              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))
              (checkParserErrors p)

              (define stmts (get program Statements))
              (check-equal? (length stmts) 1 "CHECK len(statements)")

              (define stmt (first stmts))
              (check-equal? (is-a? stmt ast.ExpressionStatement) #t)

              (define expr (get stmt Expression))
              (check-equal? (is-a? expr ast.Boolean) #t)

              (check-equal? (get expr Value) expectedBoolean)


            )
  )
); END TestBooleanExpression

(define (TestIfExpression)
  (displayln "In TestIfExpression")
  (define tests (list (list "if (x < y) { x }")))

    (for/list ([tt tests]
               [i (in-naturals 1)]) ;1, 2, 3, ...
            (begin
              (define input  (first  tt))

              (define l (lexer.New input))
              (define p (parser.New l))
              (define program (send p ParseProgram ))
              (checkParserErrors p)


              (define stmts (get program Statements))
              (check-equal? (length stmts) 1 "CHECK len(statements)")

              (define stmt (first stmts))
              (check-equal? (is-a? stmt ast.ExpressionStatement) #t)

              (define expr (get stmt Expression))
              (check-equal? (is-a? expr ast.IfExpression) #t)

              (testInfixExpression (get expr Condition) "x" "<" "y")

              (check-equal? (length (get-nested-2 expr Consequence Statements)) 1)

              (define consequence (first (get-nested-2 expr Consequence Statements)))
              (check-equal? (is-a? consequence ast.ExpressionStatement) #t)

              (testIdentifier (get consequence Expression) "x")

              (check-equal? (null? (get expr Alternative)) #t)

            )
  )

); END TestIfExpression


(define (TestIfElseExpression)
  (displayln "in TestIfElseExpression")
  (define tests (list (list "if (x < y) { x } else { y }")))

  (for/list ([tt tests]
             [i (in-naturals 1)])
    (begin
      (define input  (first  tt))

      (define l (lexer.New input))
      (define p (parser.New l))
      (define program (send p ParseProgram ))
      (checkParserErrors p)

      (define stmts (get program Statements))
      (check-equal? (length stmts) 1 "CHECK len(statements)")

      (define stmt (first stmts))
      (check-equal? (is-a? stmt ast.ExpressionStatement) #t)

      (define expr (get stmt Expression))
      (check-equal? (is-a? expr ast.IfExpression) #t)

      (testInfixExpression (get expr Condition) "x" "<" "y")

      (check-equal? (length (get-nested-2 expr Consequence Statements)) 1)

      (define consequence (first (get-nested-2 expr Consequence Statements)))
      (check-equal? (is-a? consequence ast.ExpressionStatement) #t)

      (testIdentifier (get consequence Expression) "x")

      (check-equal? (length (get-nested-2 expr Alternative Statements)) 1)

      (define alternative (first (get-nested-2 expr Alternative Statements)))
      (check-equal? (is-a? alternative ast.ExpressionStatement) #t)
      (testIdentifier (get alternative Expression) "y")
    )
  )

); :: END TestIfElseExpression


; :: Start helper functions


(define (testLetStatement statement name)
  (check-equal? (send statement TokenLiteral) "let" "CHECK Literal=='let'")
  (check-equal? (is-a? statement ast.LetStatement) true "CHECK class")
  (check-equal? (get-nested-2 statement Name Value) name "CHECK letStmt.Name.Value == name")
  (check-equal? (send (get statement Name) TokenLiteral) name "CHECK letstmt.Name.TokenLiteral() == name")
)



(define (testLiteralExpression expr expected)
  ;; (printf "in testLiteralExpression\n")
  ;; (printf "expected : ~a\n" expected )
  ;; (printf "number? : ~a\n" (number? expected))
  ;; (printf "string? : ~a\n" (string? expected))
  ;; (printf "boolean? : ~a\n" (boolean? expected))

  (cond
    [(number? expected) (testIntegerLiteral expr expected)]
    [(string? expected) (testIdentifier expr expected)]
    [(boolean? expected) (testBooleanLiteral expr expected)]
    [else (fail (format "type of exp not handle. given ~a\n" expected))]
  )
)


(define (testIntegerLiteral il value)
  ;; (printf "in testIntegerLiteral\n")
  (displayln il)
  (check-equal? (is-a? il ast.IntegerLiteral) true)
  (check-equal? (get il Value) value)
  (check-equal? (send il TokenLiteral) (format "~a" value))
)

(define (testIdentifier expr value)
  (check-equal? (is-a? expr ast.Identifier) true)
  (check-equal? (get expr Value) value)
  (check-equal? (send expr TokenLiteral) value)
)

(define (testBooleanLiteral expr value)
  (check-equal? (is-a? expr ast.Boolean) true)
  (check-equal? (get expr Value) value)
  (check-equal? (send expr TokenLiteral) (cond [value "true"] [(not value) "false"]))
)

(define (checkParserErrors p)
  (define errors (send p Errors))
  (unless (empty? errors)
      (fail (format "parser has ~a errors \n parser error: ~a"
                    (length errors)                      errors)
      )
  )
)

(define (testInfixExpression expr left operator right)
  (check-equal? (is-a? expr ast.InfixExpression) true)
  (testLiteralExpression (get expr Left) left)
  (check-equal? (get expr Operator) operator)
  (testLiteralExpression (get expr Right) right)
)

;; RUNNING TESTS
(TestLetStatements)
(TestReturnStatements)
(TestIdentifierExpression)
(TestIntegerLiteralExpression)
(TestParsingPrefixExpressions)
(TestParsingInfixExpressions)
(TestOperatorPrecedenceParsing)
(TestBooleanExpression)
(TestIfExpression)
(TestIfElseExpression)
