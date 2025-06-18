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


(define (TestBooleanExpression)
  (define tests '(
                  ("true" #t)
                  ("false" #f)
                  ("1 < 2" #t)
                  ("1 > 2" #f)
                  ("1 < 1" #f)
                  ("1 > 1" #f)
                  ("1 == 1" #t)
                  ("1 != 1" #f)
                  ("1 == 2" #f)
                  ("1 != 2" #t)
                  ("true == true" #t)
                  ("false == false" #t)
                  ("true == false" #f)
                  ("true != false" #t)
                  ("false != true" #t)
                  ("(1 < 2) == true" #t)
                  ("(1 < 2) == false" #f)
                  ("(1 > 2) == true" #f)
                  ("(1 > 2) == false" #t)))


  (for/list ([tt tests]
             [i (in-naturals 0)])
    (define input (first tt))
    (define expected (second tt))

    (printf "i: ~a\n" i)
    (printf "tt: ~a\n" tt)

    (define evaluated (testEval input))
    (testBooleanObject evaluated expected)))

(define (TestBangOperator)
  (define tests '(  
                  ("!true" #f)
                  ("!false" #t)
                  ("!5" #f)
                  ("!!true" #t)
                  ("!!false" #f)
                  ("!!5" #t)))

  (for/list ([tt tests]
             [i (in-naturals 0)])
    
    (define input (first tt))
    (define expected (second tt))

    (define evaluated (testEval input))
    (testBooleanObject evaluated expected)))
                    
(define (TestIfElseExpressions)
  (define tests '( 
                    ("if (true) { 10 }" 10)
                    ("if (false) { 10 }" ()) ;; null
                    ("if (1) { 10 }" 10)
                    ("if (1 < 2) { 10 }" 10)
                    ("if (1 > 2) { 10 }" ()) ;; null 
                    ("if (1 > 2) { 10 } else { 20 }" 20)
                    ("if (1 < 2) { 10 } else { 20 }" 10)))

  (for/list ([tt tests]
             [i (in-naturals 0)])

    (define input (first tt))
    (define expected (second tt))

    (printf "i: ~a\n" i)
    (printf "tt: ~a\n" tt)
  
    (define evaluated (testEval input))
    
    (if (null? expected)
      (testNullObject evaluated)
      (testIntegerObject evaluated expected))))
                    
(define (TestReturnStatements)

  (define tests '( 
                  ("return 10;" 10)
                  ("return 10; 9;" 10)
                  ("return 2 * 5; 9;" 10)
                  ("9; return 2 * 5; 9;" 10)
                  ("if (10 > 1) { return 10; }" 10)
                  ("if (10 > 1) {
                    if (10 > 1) {
                      return 10;
                    }

                    return 1;
                    }"
                               10)
    
                  ("let f = fn(x) {
                    return x;
                    x + 10;
                    };
                    f(10);"
                               10)
    
                  ("	let f = fn(x) {
                    let result = x + 10;
                    return result;
                    return 10;
                    };
                    f(10);"
                               20)))

  (for/list ([tt tests]
             [i (in-naturals 0)])
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

(define (testBooleanObject obj expected)
  (check-equal? (is-a? obj object.Boolean) #t) "is-a? object.Boolean"
  (check-equal? (get obj Value) expected) "obj.Value =? expected")

(define (testNullObject obj)
  (check-equal? (null? obj) #t) "null? obj")

(define (testEval input)
  (define l (lexer.New input))
  (define p (parser.New l))
  (define program (send p ParseProgram))
  (define env (object.NewEnvironment))
  (evaluator.Eval program env))

; local macros
(define-syntax get
  (syntax-rules ()
    [(get obj f)
     (get-field f obj)]
    [(get obj f1 f2)
     (get (get-field f1 obj) f2)]))

; Test Calls
(TestEvalIntegerExpression)
(TestBooleanExpression)
(TestBangOperator)
(TestIfElseExpressions)
(TestReturnStatements)

