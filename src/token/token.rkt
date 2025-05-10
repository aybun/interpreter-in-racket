#lang racket
;(require racket/class)

; dzpower in 
; https://www.reddit.com/r/Racket/comments/cm1qe1/define_multiple_variables_at_once/
(define-syntax-rule (define-mult-val (a b) ...)
  (match-define (list a ...) (list b ...)))


(define-mult-val
  (ILLEGAL "ILLEGAL")
	(EOF     "EOF")

	; Identifiers + literals
	(IDENT "IDENT") ;add, foobar, x, y, ...
	(INT   "INT")   ;1343456

	;Operators
	(ASSIGN    "=")
	(PLUS      "+")
	(MINUS     "-")
	(BANG      "!")
	(ASTERISK  "*")
	(SLASH     "/")

	(LT "<")
	(GT ">")

	(EQ     "==")
	(NOT_EQ "!=")

	;Delimiters
	(COMMA     ",")
	(SEMICOLON ";")

	(LPAREN "(")
	(RPAREN ")")
	(LBRACE "{")
	(RBRACE "}")

	;Keywords
	(FUNCTION "FUNCTION")
	(LET      "LET")
	(TRUE     "TRUE")
	(FALSE    "FALSE")
	(IF       "IF")
	(ELSE     "ELSE")
	(RETURN   "RETURN")
)


(define Token
  (class object%
    (init-field Type Literal)
    (super-new)))

(define keywords (hasheq 
                        "fn" FUNCTION
                        "let" LET
                        "true" TRUE
                        "false"  FALSE
                        "if"     IF
                        "else"   ELSE
                        "return" RETURN
                         ))

(define (LookupIdent ident)
  (define tok (hash-ref keywords ident null))
  (if (null? tok)
    IDENT
    tok
  )
)

(provide (all-defined-out))


