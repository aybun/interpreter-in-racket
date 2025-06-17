#lang racket


(require rackunit)
(require dyoo-while-loop)

(require (prefix-in lexer. "../lexer/lexer.rkt"))
(require (prefix-in object. "../object/object.rkt"))
(require (prefix-in object. "../object/environment.rkt"))
(require (prefix-in parser. "../parser/parser.rkt"))
(require (prefix-in evaluator. "../evaluator/evaluator.rkt"))



(define (TestEvalIntegerExpression)
  (define tests '(
                  ("5" 5)
                  ("10" 10)
                  ("-5" -5)
                  ("-10" -10)
                  ("5 + 5 + 5 + 5 - 10" 10)
                  ("2 * 2 * 2 * 2 * 2" 32)
                  ("-50 + 100 + -50" 0)
                  ("5 * 2 + 10" 20)
                  ("5 + 2 * 10" 25)
                  ("20 + 2 * -10" 0)
                  ("50 / 2 * 2 + 10" 60)
                  ("2 * (5 + 10)" 30)
                  ("3 * 3 * 3 + 10" 37)
                  ("3 * (3 * 3) + 10" 37)
                  ("(5 + 10 * 2 + 15 / 3) * 2 + -10" 50)))

  (for/list ([i (in-naturals 0)]
             [tt tests])

    (define input (first tt))
    (define expected (second tt))
    
    (printf "i: ~a\n" i)
    (printf "tt: ~a\n" tt)

    (define evaluated (testEval input))
    (testIntegerObject evaluated expected)))


(define (testIntegerObject obj expected)
  (printf "in testIntegerObject\n")
  ;; (printf "obj: ~a, obj.Value: ~a\n" obj (get obj Value))
  ;; (printf "obj: ~a\nexpected: ~a\n" obj expected)

  (check-equal? (is-a? obj object.Integer) #t "is-a? object.Integer")
  (check-equal? (get obj Value) expected) "obj.Value =? expected")


(define (testEval input)
  (define l (lexer.New input))
  (define p (parser.New l))
  (define program (send p ParseProgram))
  (define env (object.NewEnvironment))
  (evaluator.Eval program env))

; Helper Zone
(define-syntax get
  (syntax-rules ()
    [(get obj f)
     (get-field f obj)]
    [(get obj f1 f2)
     (get (get-field f1 obj) f2)]))

; Test Calls
(TestEvalIntegerExpression)
