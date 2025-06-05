#lang racket

(require rackunit)
(require (prefix-in ast.   "ast.rkt"))
(require (prefix-in token. "../token/token.rkt"))

(define (TestString)
  (define letStatement (new ast.LetStatement [Token (new token.Token [Type token.LET] [Literal "let"]) ]
                                 [Name  (new ast.Identifier
                                             [Token (new token.Token [Type token.IDENT] [Literal "myVar"])]
                                             [Value "myVar"])
                                 ]
                                 [Value (new ast.Identifier
                                             [Token (new token.Token [Type token.IDENT] [Literal "anotherVar"])]
                                             [Value "anotherVar"])
                                 ]


                       )
  )

  (define program (new ast.Program [Statements (list letStatement)]))

  (check-equal? (send program String) "let myVar = anotherVar;")
)

(TestString)
