#lang racket

(require rackunit)
(require (prefix-in token. "../token/token.rkt"))
(require (prefix-in lexer. "./lexer.rkt"))

;; https://docs.racket-lang.org/rackunit/quick-start.html

(define	input  "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
")


(define-syntax-rule (go-items->list {f0, s0} ...) 
  (list (list f0 s0) ... ))
(define tests (go-items->list 
	        {token.LET, "let"}
	        {token.IDENT, "five"}
		{token.ASSIGN, "="}
		{token.INT, "5"}
		{token.SEMICOLON, ";"}
		{token.LET, "let"}
		{token.IDENT, "ten"}
		{token.ASSIGN, "="}
		{token.INT, "10"}
		{token.SEMICOLON, ";"}
		{token.LET, "let"}
		{token.IDENT, "add"}
		{token.ASSIGN, "="}
		{token.FUNCTION, "fn"}
		{token.LPAREN, "("}
		{token.IDENT, "x"}
		{token.COMMA, ","}
		{token.IDENT, "y"}
		{token.RPAREN, ")"}
		{token.LBRACE, "{"}
		{token.IDENT, "x"}
		{token.PLUS, "+"}
		{token.IDENT, "y"}
		{token.SEMICOLON, ";"}
		{token.RBRACE, "}"}
		{token.SEMICOLON, ";"}
		{token.LET, "let"}
		{token.IDENT, "result"}
		{token.ASSIGN, "="}
		{token.IDENT, "add"}
		{token.LPAREN, "("}
		{token.IDENT, "five"}
		{token.COMMA, ","}
		{token.IDENT, "ten"}
		{token.RPAREN, ")"}
		{token.SEMICOLON, ";"}
		{token.BANG, "!"}
		{token.MINUS, "-"}
		{token.SLASH, "/"}
		{token.ASTERISK, "*"}
		{token.INT, "5"}
		{token.SEMICOLON, ";"}
		{token.INT, "5"}
		{token.LT, "<"}
		{token.INT, "10"}
		{token.GT, ">"}
		{token.INT, "5"}
		{token.SEMICOLON, ";"}
		{token.IF, "if"}
		{token.LPAREN, "("}
		{token.INT, "5"}
		{token.LT, "<"}
		{token.INT, "10"}
		{token.RPAREN, ")"}
		{token.LBRACE, "{"}
		{token.RETURN, "return"}
		{token.TRUE, "true"}
		{token.SEMICOLON, ";"}
		{token.RBRACE, "}"}
		{token.ELSE, "else"}
		{token.LBRACE, "{"}
		{token.RETURN, "return"}
		{token.FALSE, "false"}
		{token.SEMICOLON, ";"}
		{token.RBRACE, "}"}
		{token.INT, "10"}
		{token.EQ, "=="}
		{token.INT, "10"}
		{token.SEMICOLON, ";"}
		{token.INT, "10"}
		{token.NOT_EQ, "!="}
		{token.INT, "9"}
		{token.SEMICOLON, ";"}
		{token.EOF, ""}
))

(define l (lexer.New input))
(define (run-tests)
	(for/list ([tt tests]
		   [i (in-naturals)]) ;0, 1, 2, ...

		(let ([tok (send l NextToken)])
			;; (display (format "In let : ~a\ntok : ~a ~a\n" i (get-field Type tok) (get-field Literal tok)))
			(check-equal? (get-field Type tok) (first tt) "Token.Type Check")
			(check-equal? (get-field Literal tok) (second tt) "Token.Literal Check")
		)
	)
        (format "Done running ~a tests\n" (length tests))

  )
(run-tests)
