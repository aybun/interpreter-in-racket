#lang racket

(require dyoo-while-loop)

(require (prefix-in evaluator. "../evaluator/evaluator.rkt"))
(require (prefix-in lexer. "../lexer/lexer.rkt"))
(require (prefix-in object. "../object/object.rkt"))
(require (prefix-in object. "../object/environment.rkt"))
(require (prefix-in parser. "../parser/parser.rkt"))

(provide Start)

(define PROMPT ">> ")

(define (Start in out)
  (define env (object.NewEnvironment)) 

  (while #t
    (fprintf out "~a" PROMPT)
    (flush-output out)

    (define line (read-line in))
    (cond
      [(eof-object? line) (break)]
      [else (begin
                (define l (lexer.New line))
                (define p (parser.New l))
                (define program (send p ParseProgram))
                (define errors (send p Errors))
                (when (not (empty? errors))
                  (printParserErrors out errors)
                  (continue))

                (define evaluated (evaluator.Eval program env))
                (when (not (null? evaluated))
                  (fprintf out "~a\n" (send evaluated Inspect))))])))

(define MONKEY_FACE "            __,__
   .--.  .-\"     \"-.  .--.
  / .. \\/  .-. .-.  \\/ .. \\
 | |  '|  /   Y   \\  |'  | |
 | \\   \\  \\ 0 | 0 /  /   / |
  \\ '- ,\\.-\"\"\"\"\"\"\"-./,  -' /
   ''-' /_   ^ ^   _\\ '-''
       |  \\._   _./  |
       \\   \\ '~' /   /
        '._ '-=-' _.'
           '-----'
")

(define (printParserErrors out errors)
  (fprintf out "~a" MONKEY_FACE)
  (fprintf out "Woops! We ran into some monkey business here!\n")
  (fprintf out " parser errors:\n")

  (for/list ([msg errors])
    (fprintf out "~a" (append
                        "\t"
                        msg
                        "\n"))))
