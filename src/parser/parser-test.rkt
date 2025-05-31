#lang racket

(require rackunit)
(require (prefix-in token. "../token/token.rkt"))
(require (prefix-in lexer. "../lexer/lexer.rkt"))
(require (prefix-in ast.   "../ast/ast.rkt"))
(require (prefix-in parser. "parser.rkt"))


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
      '("let x = 5;" "x" 5)
      '("let y = true;" "y" true)
      '("let foobar = y;" "foobar" "y")
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

              (define stmts (get program Statements))

              (checkParserErrors p)
              (check-equal? (length (stmts))              1 "CHECK len(statements)")
              ;; (check-type)

              (define stmt (first stmts))
              (testLetStatement (stmt)      expectedIdentifier)
              (testLiteralExpression (get stmt Value) expectedValue)

            );; ::END BEGIN
  );; :: END FOR

)



(define (testLetStatement statement name)
  (check-equal? (send statement TokenLiteral) "let" "CHECK Literal=='let'")
  (check-equal? (is-a? statement ast.LetStatement) true "CHECK class")
  (check-equal? (get-nested-2 statement Name Value) name "CHECK letStmt.Name.Value == name")
  (check-equal? (send (get statement Name) TokenLiteral) name "CHECK letstmt.Name.TokenLiteral() == name")
)



(define (testLiteralExpression exp expected)
  (cond
    [(number? expected) (testIntegerLiteral exp expected)]
    [(string? expected) (testIdentifier exp expected)]
    [(boolean? expected) (testBooleanLiteral exp expected)]
    [else (fail "type of exp not handle. ")]
  )
)


(define (testIntegerLiteral il value)
  (check-equal? (is-a? il ast.IntegerLiteral) true)
  (check-equal? (get il Value) value)
  (check-equal? (send il TokenLiteral) value)
)

(define (testIdentifier expr value)
  (check-equal? (is-a? expr ast.Identifier) true)
  (check-equal? (get expr Value) value)
  (check-equal? (send expr TokenLiteral))
)

(define (testBooleanLiteral exp value)
  (check-equal? (is-a? exp ast.Boolean) true)
  (check-equal? (get exp Value) value)
  (check-equal? (send exp TokenLiteral) value)
)

(define (checkParserErrors p)
  (define errors (send p Errors))
  (unless (empty? errors)
      (fail (format "parser has ~a errors \n parser error: ~a"
                    (length errors)                      errors)
      )
  )
)


;; RUNNING TESTS
(TestLetStatements)
