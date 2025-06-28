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
      
(define (TestErrorHandling)
  (define tests '( 
                      (
                       "5 + true;"
                       "type mismatch: INTEGER + BOOLEAN")
      
                      (
                       "5 + true; 5;"
                       "type mismatch: INTEGER + BOOLEAN")
      
                      (
                       "-true"
                       "unknown operator: -BOOLEAN")
      
                      (
                       "true + false;"
                       "unknown operator: BOOLEAN + BOOLEAN")
      
                      (
                       "true + false + true + false;"
                       "unknown operator: BOOLEAN + BOOLEAN")
      
                      (
                       "5; true + false; 5"
                       "unknown operator: BOOLEAN + BOOLEAN")
      
                      (
                       "if (10 > 1) { true + false; }"
                       "unknown operator: BOOLEAN + BOOLEAN")
      
                      (
                       "
                      if (10 > 1) {
                      if (10 > 1) {
                          return true + false;
                      }

                      return 1;
                      }
                      "
                       "unknown operator: BOOLEAN + BOOLEAN")
      
                      (
                       "foobar"
                       "identifier not found: foobar")))
    
  (for/list ([tt tests]
             [i (in-naturals 0)])                    
    (define input (first tt))
    (define expectedMessage (second tt))
    
    (define evaluated (testEval input))

    (check-equal? (is-a? evaluated object.Error) #t "is-a? object.Error")
    (check-equal? (get evaluated Message) expectedMessage)))

(define (TestLetStatements)
  (define tests '( 
                    ("let a = 5; a;" 5)
                    ("let a = 5 * 5; a;" 25)
                    ("let a = 5; let b = a; b;" 5)
                    ("let a = 5; let b = a; let c = a + b + 5; c;" 15)))

  (for/list ([tt tests]
             [i (in-naturals 0)])
    
    (define input (first tt))
    (define expected (second tt))
    (testIntegerObject (testEval input) expected)))                 

                    
(define (TestFunctionObject)
  (define input "fn(x) { x + 2; };")
  (define expectedBody "(x + 2)")
  
  (define evaluated (testEval input))

  (check-equal? (is-a? evaluated object.Function) #t "is-a? object.Function")

  (define params (get evaluated Parameters))
  (check-equal? (length params) 1)
  (check-equal? (send (first params) String) "x"))


(define (TestFunctionApplication)
  (define tests '( 
                    ("let identity = fn(x) { x; }; identity(5);" 5)
                    ("let identity = fn(x) { return x; }; identity(5);" 5)
                    ("let double = fn(x) { x * 2; }; double(5);" 10)
                    ("let add = fn(x, y) { x + y; }; add(5, 5);" 10)
                    ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));" 20)
                    ("fn(x) { x; }(5)" 5))) 

  (for/list ([tt tests]
             [i (in-naturals 0)])
    
    (define input (first tt))
    (define expected (second tt))

    (testIntegerObject (testEval input) expected)))
                   

(define (TestEnclosingEnvironments)

  (define input "

      let first = 10;
      let second = 10;
      let third = 10;

      let ourFunction = fn(first) {
        let second = 20;

        first + second + third;
      };

      ourFunction(20) + first + second;

    ")

  (testIntegerObject (testEval input) 70))

(define (TestClosures)
  (define input "
        let newAdder = fn(x) {
          fn(y) { x + y };
        };

        let addTwo = newAdder(2);
        addTwo(2);
            ")
  
  (testIntegerObject (testEval input) 4))

(define (TestStringLiteral)
  (define input "\"Hello World!\"")
  
  (define evaluated (testEval input))
  (check-equal? (is-a? evaluated object.String) #t "is-a? object.String")

  (printf "evaluated.Value: ~a\n" (get evaluated Value))
  (check-equal? (get evaluated Value) "Hello World!"))

(define (TestStringConcatenation)
  (define input "\"Hello\" + \" \" + \"World!\"")
  (define evaluated (testEval input))
  (check-equal? (is-a? evaluated object.String) #t "is-a? object.String")

  (printf "evaluated.Value: ~a\n" (get evaluated Value))
  (check-equal? (get evaluated Value) "Hello World!"))

(define (TestBuiltinFunctions)
  (printf "in TestBuiltinFunctions")
  (define tests '(
                  ("len(\"\")" 0)
                  ("len(\"four\")" 4)
                  ("len(\"hello world\")" 11)
                  ("len(1)" "argument to `len` not supported, got INTEGER")
                  ("len(\"one\", \"two\")" "wrong number of arguments. got=2, want=1")
                  ("len([1, 2, 3])" 3)
                  ("len([])" 0)
                  ("puts("hello", "world!")" ())
                  ("first([1, 2, 3])" 1)
                  ("first([])" ())
                  ("first(1)" "argument to `first` must be ARRAY, got INTEGER")
                  ("last([1, 2, 3])" 3)
                  ("last([])" ())
                  ("last(1)" "argument to `last` must be ARRAY, got INTEGER")
                  ("rest([1, 2, 3])" (2 3))
                  ("rest([])" ())
                  ("push([], 1)" (1))
                  ("push(1, 1)" "argument to `push` must be ARRAY, got INTEGER")
                  ))

  (for/list ([tt tests]
             [i (in-naturals 0)])

    (define input (first tt))
    (define expected (second tt))
    (define evaluated (testEval input))

    (printf "i: ~a\n" i)
    (printf "evaluated: ~a\n" evaluated)
    (printf "expected: ~a\n" expected)
    (when (is-a? evaluated object.Error) (printf "ERROR MESSAGE: ~a\n" (get evaluated Message)))

    (cond
      [(integer? expected) (testIntegerObject evaluated expected)]
      [(null? expected) (testNullObject evaluated)]
      [(string? expected) (begin
                            (check-equal? (is-a? evaluated object.Error) #t "is-a? object.Error")
                            (check-equal? (get evaluated Message) expected))]
      [(list? expected) (begin
                          (check-equal? (is-a? evaluated object.Array) #t "is-a? object.Array")
                          (check-equal? (length (get evaluated Elements)) (length expected) "len")
                          (for/list ([intObj (get evaluated Elements)]
                                     [ex expected])
                            (testIntegerObject intObj ex))
                          ) ])))


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
(TestErrorHandling)
(TestLetStatements)
(TestFunctionObject)
(TestFunctionApplication)
(TestEnclosingEnvironments)
(TestClosures)
(TestStringLiteral)
(TestStringConcatenation)
(TestBuiltinFunctions)
