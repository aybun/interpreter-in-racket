#lang racket

(require (prefix-in token. "../token/token.rkt"))
(require (prefix-in lexer. "../lexer/lexer.rkt"))


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
(define Token
  (class object%
    (init-field l errors curToken peekToken prefixParseFns infixParseFns)
    (super-new)))



;; (define (New l)
;;   ()

;;   )
