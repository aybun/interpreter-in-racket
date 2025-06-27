#lang racket

(require dyoo-while-loop)
(require (prefix-in ast. "../ast/ast.rkt"))
(require (prefix-in object. "../object/object.rkt"))
(require (prefix-in object. "../object/environment.rkt"))

(provide NULL TRUE FALSE Eval)

(define NULL (new object.Null))
(define TRUE (new object.Boolean [Value #t]))
(define FALSE (new object.Boolean [Value #f]))


(define (Eval node env)
   (printf "in Eval\n")
   (printf "node : ~a\n" node)
   (printf "node is ReturnStatement : ~a\n" (is-a? node ast.ReturnStatement))
   (printf "node is FunctionLiteral : ~a\n" (is-a? node ast.FunctionLiteral))
   (printf "node is Identifier : ~a\n" (is-a? node ast.Identifier))

   (cond
      [(is-a? node ast.Program ) (evalProgram node env)]
      [(is-a? node ast.BlockStatement) (evalBlockStatement node env)]
      [(is-a? node ast.ExpressionStatement) (Eval (get node Expression) env)]
      [(is-a? node ast.ReturnStatement) (begin
                                           (define val (Eval (get node ReturnValue) env))
                                           (if (isError val)
                                               val
                                               (new object.ReturnValue [Value val])))]
      [(is-a? node ast.LetStatement) (begin
                                         (define val (Eval (get node Value) env))
                                         (if (isError val)
                                             val
                                             (begin
                                                 (send env Set (get node Name Value) val)
                                                 null)))]
      [(is-a? node ast.IntegerLiteral) (new object.Integer [Value (get node Value)])]
      [(is-a? node ast.StringLiteral) (new object.String [Value (get node Value)])]
      [(is-a? node ast.Boolean) (nativeBoolToBooleanObject (get node Value))]
      [(is-a? node ast.PrefixExpression) (begin
                                           (define right (Eval (get node Right) env))
                                           (if (isError right)
                                               right
                                               (evalPrefixExpression (get node Operator) right)))]
      [(is-a? node ast.InfixExpression) (begin
                                           (define returnValue null)
                                           (while #t
                                             (begin
                                                (define left (Eval (get node Left) env))
                                                (when (isError left) (set! returnValue left) (break))

                                                (define right (Eval (get node Right) env))
                                                (when (isError right) (set! returnValue right) (break))

                                                (set! returnValue (evalInfixExpression (get node Operator) left right))
                                                (break)))
                                           returnValue)]
      [(is-a? node ast.IfExpression) (evalIfExpression node env)]
      [(is-a? node ast.Identifier) (evalIdentifier node env)]
      [(is-a? node ast.FunctionLiteral) (new object.Function [Parameters (get node Parameters)] [Env env] [Body (get node Body)])]
      [(is-a? node ast.CallExpression) (begin
                                         (define returnValue null)
                                         (while #t
                                           (begin
                                               (define fun (Eval (get node Function) env))
                                               (when (isError fun) (set! returnValue fun) (break))

                                               (define args (evalExpressions (get node Arguments) env))
                                               (when
                                                    (and
                                                        (equal? (length args) 1)
                                                        (isError (first args)))
                                                    (set! returnValue (first args))
                                                    (break))

                                               (set! returnValue (applyFunction fun args))
                                               (break)))
                                         returnValue)]
      [(is-a? node ast.ArrayLiteral) (begin
                                       (define elements (evalExpressions (get node Elements) env))
                                       (if (and (equal? (length elements) 1)
                                                (isError (first elements)))
                                           (first elements)
                                           (new object.Array [Elements elements])))]
      [(is-a? node ast.IndexExpression) (begin
                                          (define returnValue null)
                                          (while #t (begin

                                                     (define left (Eval (get node Left) env))
                                                     (when (isError left)
                                                       (set! returnValue left)
                                                       (break))

                                                     (define index (Eval (get node Index) env))

                                                     (when (isError index)
                                                       (set! returnValue index)
                                                       (break))

                                                     (set! returnValue (evalIndexExpression left index))

                                                     (break)))
                                          returnValue)]
      [(is-a? node ast.HashLiteral) (evalHashLiteral node env)]

      [else null]))


(define (evalProgram program env)
  (printf "in evalProgram\n")
  (define result null)
  (define break-loop #f)
  (for/list ([statement (get program Statements)]
             [i (in-naturals 0)]
             #:break break-loop)
      (set! result (Eval statement env))
      (printf "in loop of evalProgram\n")
      (printf "i: ~a\n" i)
      (printf "result: ~a\n" result)
      (printf "is-a? object.ReturnValue: ~a\n" (is-a? result object.ReturnValue))
      (printf "is-a? object.Error: ~a\n" (is-a? result object.Error))

      (cond
        [(is-a? result object.ReturnValue) (begin
                                             (set! result (get result Value))
                                             (set! break-loop #t))]
        [(is-a? result object.Error) (begin
                                       (set! result result)
                                       (set! break-loop #t))]))
  (printf "before return in evalProgram\n")
  (printf "result : ~a\n" result)
  result)


(define (evalBlockStatement block env)
  (define result null)
  (define break-loop #f)
  (for/list ([statement (get block Statements)]
             #:break break-loop)
      (set! result (Eval statement env))
      (when (not (null? result))
            (define rt (send result Type))
            (when (or (equal? rt object.RETURN_VALUE_OBJ) (equal? rt object.ERROR_OBJ))
                (set! break-loop #t))))

  result)

(define (nativeBoolToBooleanObject input)
  (printf "in nativeBoolToBooleanObject\n")
  (printf "input: ~a\n" input)

  (if input
      TRUE
      FALSE))


(define (evalPrefixExpression operator right)
    (case operator
        [("!") (evalBangOperatorExpression right)]
        [("-") (evalMinusPrefixOperatorExpression right)]
        [else (newError "unknown operator ~a~a" operator (send right Type))]))

(define (evalInfixExpression operator left right)
    (cond
        [ (and (equal? (send left Type) object.INTEGER_OBJ)
               (equal? (send right Type) object.INTEGER_OBJ))
          (evalIntegerInfixExpression operator left right)]

        [ (and (equal? (send left Type) object.STRING_OBJ)
               (equal? (send right Type) object.STRING_OBJ))
          (evalStringInfixExpression operator left right)]

        [ (equal? operator "==")
          (nativeBoolToBooleanObject (equal? left right))]

        [ (equal? operator "!=")
          (nativeBoolToBooleanObject (not (equal? left right)))]

        [ (not (equal? (send left Type) (send right Type)))
          (newError "type mismatch: ~a ~a ~a" (send left Type) operator (send right Type))]

        [ else
          (newError "unknown operator: ~a ~a ~a" (send left Type) operator (send right Type))]))

(define (evalBangOperatorExpression right)
    (cond
        [(equal? right TRUE) FALSE]
        [(equal? right FALSE) TRUE]
        [(equal? right NULL) TRUE]
        [else FALSE]))


(define (evalMinusPrefixOperatorExpression right)
    (if (not (equal? (send right Type) object.INTEGER_OBJ))
        (newError "unknown operator: -~a" (send right Type))
        (new object.Integer [Value (- (get right Value))])))


(define (evalIntegerInfixExpression operator left right)
    (define leftVal (get left Value))
    (define rightVal (get right Value))
    (case operator
        [("+")  (new object.Integer [Value (+ leftVal rightVal)])]
        [("-")  (new object.Integer [Value (- leftVal rightVal)])]
        [("*")  (new object.Integer [Value (* leftVal rightVal)])]
        [("/")  (new object.Integer [Value (quotient leftVal rightVal)])]
        [("<")  (nativeBoolToBooleanObject (< leftVal rightVal))]
        [(">")  (nativeBoolToBooleanObject (> leftVal rightVal))]
        [("==") (nativeBoolToBooleanObject (equal? leftVal rightVal))]
        [("!=") (nativeBoolToBooleanObject (not (equal? leftVal rightVal)))]
        [else   (newError "unknown operator: ~a ~a ~a" (send left Type) operator (send right Type))]))

(define (evalStringInfixExpression operator left right)
  (if (not (equal? operator "+"))
    (newError "unknown operator: ~a ~a ~a" (send left Type) operator (send right Type))
    (new object.String [Value (string-append (get left Value) (get right Value))])))
(define (evalIfExpression ie env)
  (printf "in evalIfExpression\n")
  (define returnValue null)
  (while #t (begin
              (define condition (Eval (get ie Condition) env))
              (when (isError condition) (set! returnValue condition) (break))

              (set! returnValue (cond
                                  [(isTruthy condition) (Eval (get ie Consequence) env)]
                                  [(not (null? (get ie Alternative))) (Eval (get ie Alternative) env)]
                                  [else null]))
              (break)))
  (printf "before return\n")
  (printf "returnValue: ~a\n" returnValue)
  returnValue)


(define (evalIdentifier node env)
    (define returnedList (send env Get (get node Value)))
    (define val (first returnedList))
    (define ok  (second returnedList))

    (if (not ok)
        (begin
           (printf "node.Value ~a\n" (get node Value))
           (newError (format "identifier not found: ~a" (get node Value))))
        val))


(define (isTruthy obj)
    (cond
        [(equal? obj NULL)   #f]
        [(equal? obj TRUE)   #t]
        [(equal? obj FALSE)  #f]
        [else                 #t]))


(define (newError formatPattern . xs)
    (new object.Error [Message (apply format formatPattern xs)]))


(define (isError obj)
    (if (not (null? obj))
        (equal? (send obj Type) object.ERROR_OBJ)
        #f))

(define (evalExpressions exps env)
    (define returnValue (list))
    (define break-loop #f)
    (define evaluated null)
    (for/list ([e exps]
               #:break break-loop)
        (begin
            (set! evaluated (Eval e env))
            (if (isError evaluated)
                (begin (set! returnValue (list evaluated)) (set! break-loop #t))
                (set! returnValue (append returnValue (list evaluated))))))

    returnValue)

(define (applyFunction fn args)
    (define returnValue null)
    (while #t (begin
                  (when (not (is-a? fn object.Function))
                        (set! returnValue (newError "not a function: ~a" (send fn Type)))
                        (break))
                  (define extendedEnv (extendFunctionEnv fn args))
                  (define evaluated (Eval (get fn Body) extendedEnv))
                  (set! returnValue (unwrapReturnValue evaluated))
                  (break)))
    returnValue)
(define (extendFunctionEnv fn args)
    (define env (object.NewEnclosedEnvironment (get fn Env)))

    (for/list ( [i (in-naturals 0)]
                [param (get fn Parameters)])
        (send env Set (get param Value) (list-ref args i)))

    env)

(define (unwrapReturnValue obj)
    (if (is-a? obj object.ReturnValue)
        (get obj Value)
        obj))

(define (evalIndexExpression left index)
  (cond
    [(and (equal? (send left Type) (object.ARRAY_OBJ))
          (equal? (send index Type) (object.INTEGER_OBJ))) (evalArrayIndexExpression left index)]
    [(equal? (send left Type) object.HASH_OBJ) (evalHashIndexExpression left index)]
    [else (newError "index operator not supported: ~a" (send left Type))]))

(define (evalArrayIndexExpression array index)
  (define idx (get index Value))
  (define max (- (length (get array Elements)) 1))

  (if (or (< idx 0) (> idx max))
      null
      (list-ref array idx)))

(define (evalHashLiteral node env)
  (define pairs (make-hash '()))
  (define key null)
  (define value null)
  (define returnValue null)

  (hash-for-each
   (get node Pairs)
   (lambda (keyNode valueNode)
     (while #t (begin
                 (set! key (Eval keyNode env))

                 (when (isError key)
                   (set! returnValue key)
                   (break))

                 (when (not (implementation? key object.Hashable))
                   (set! returnValue
                         (newError "unusable as hash key: ~a" (send key Type)))
                   (break))

                 (set! value (Eval valueNode env))

                 (when (isError value)
                   (set! returnValue value)
                   (break))

                 (hash-set! pairs (send key HashKey) (new object.HashPair [Key key] [Value value]))

                 (break)))))
  returnValue)

(define (evalHashIndexExpression hash-table index)
  (define returnValue null)

  (while #t
    (begin
      (when (not (implementation? index object.Hashable))
        (set! returnValue (newError "unusable as hash key: ~a" (send index Type)))
        (break))
      (define pair (hash-ref (get hash-table Pairs) (send index HashKey) null))

      (if (null? pair)
          (set! returnValue null)
          (set! returnValue (get pair Value)))
      (break)))
  returnValue)


; Helper Zone

;; Write the general case when ready.
;; Maybe this is enough.
(define-syntax get
  (syntax-rules ()
    [(get obj f)
     (get-field f obj)]
    [(get obj f1 f2)
     (get (get-field f1 obj) f2)]))
