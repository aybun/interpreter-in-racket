#lang racket

(require dyoo-while-loop)
(require (prefix-in ast. "../ast/ast.rkt"))
(require (prefix-in object. "../object/object.rkt"))


;; Write the general case when ready.
;; Maybe this is enough.
(define-syntax get
  (syntax-rules ()
    [(get obj f)
     (get-field f obj)]
    [(get obj f1 f2)
     (get (get-field f1 obj) f2)]))

(define NULL (new object.NULL))
(define TRUE (new object.TRUE [Value #t]))
(define FALSE (new object.FALSE [Value #f]))


(define (Eval node env)
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
                                         (define val (get node Value))
                                         (if (isError val)
                                             val
                                             (begin 
                                                 (send env Set (get node Name Value) val)
                                                 null)))]
      [(is-a? node ast.IntegerLiteral) (new object.Integer [Value (get node Value)])]
      [(is-a? node ast.Boolean) (nativeBoolToBooleanObject (get node Value))]
      [(is-a? node ast.PrefixExpression) (begin
                                             (define right (Eval (get node Right)) env)
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
      [(is-a? node ast.Identifier (evalIdentifier node env))]
      [(is-a? node ast.FunctionLiteral (new object.Function [Parameters (get node Paramenters)] [Env env] [Body (get node Body)]))]
      [(is-a? node ast.CallExpression) (begin
                                         (define returnValue null)
                                         (while #t
                                           (begin
                                               (define fun (Eval (get node Function) env))
                                               (when (isError fun) (set! returnValue fun) (break))

                                               (define args (evalExpressions (get node Arguments) env))
                                               (when (and
                                                       (equal? (length args) 1)
                                                       (isError (first args))
                                                       (set! returnValue (first args))
                                                       (break)))

                                               (set! returnValue (applyFunction fun args))
                                               (break)))
                                         returnValue)]))

(define (evalProgram program env)
  (define returnValue null)
  (define (break-loop #f))
  (for/list ([statement (get program Statements)]
             (#:break break-loop))
      (define result (Eval statement env))
      (cond
          [(is-a? result object.ReturnValue) (begin
                                                 (set! returnValue (get result Value))
                                                 (set! break-loop #t))]
          [(is-a? result object.Error) (begin
                                          (set! returnValue result)
                                          (set! break-loop #t))]))

  returnValue)


(define (evalBlockStatement block env)
  (define returnValue null)
  (define (break-loop #f))
  (for/list ([statement (get block Statements)])
      (set! returnValue (Eval statement env))
      (when (not (null? returnValue))
          (begin
              (define rt (send returnValue Type))
              (when (or (equal? rt object.RETURN_VALUE_OBJ) (equal? rt object.ERROR_OBJ))
                  (set! break-loop #t)))))
  
  returnValue)

(define (nativeBoolToBooleanObject input)
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

        [ (equal? operator "==")
          (nativeBoolToBooleanObject (equal? left right))]
        
        [ (not (equal? (send left Type) (send right Type)))
          (newError "type mismatch: ~a ~a ~a" (send left Type) operator (send right Type))]

        [ else 
          (newError "unknown operator: ~a ~a ~a" (send left Type) operator (send right Type))]))

(define (evalBangOperatorExpression right)
    (cond
        [(equal? right TRUE) FALSE]
        [(equal? right FALSE) TRUE]
        [(equal? right NULL) TRUE]
        [(else) FALSE]))


(define (evalMinusPrefixOperatorExpression right)
    (if (not (equal? (send right Type) object.INTEGER_OBJ))
        (newError "unknown operator: -~a" (send right Type))
        (new object.Integer [Value (- (send right Value))])))


(define (evalIntegerInfixExpression operator left right)
    (define leftVal (get left Value))
    (define rightVal (get right Value))
    (case operator
        [("+"   (new object.Integer [Value (+ leftVal rightVal)]))]
        [("-")  (new object.Integer [Value (- leftVal rightVal)])]
        [("*")  (new object.Integer [Value (* leftVal rightVal)])]
        [("/")  (new object.Integer [Value (quotient leftVal rightVal)])]
        [("<")  (nativeBoolToBooleanObject (< leftVal rightVal))]
        [(">")  (nativeBoolToBooleanObject (> leftVal rightVal))]
        [("=="  (nativeBoolToBooleanObject (equal? leftVal rightVal)))]
        [("!="  (nativeBoolToBooleanObject (not (equal? leftVal rightVal))))]
        [else   (newError "unknown operator: ~a ~a ~a" (send left Type) operator (send right Type))]))

(define (evalIfExpression ie env)
    (define returnValue null)
    (while #t (begin 
                 (define condition (Eval (get ie Condition) env))
                 (when (isError condition) (set! returnValue condition) (break))
        
                 (set! returnValue (cond
                                       [(isTruthy condition) (Eval (send ie Consequence) env)]
                                       [(not (null? (get ie Alternative))) (Eval (send ie Alternative) env)]
                                       [else null]))
                 (break)))
    returnValue)


(define (evalIdentifier node env)
    (define returnedList (send env Get (get node Value)))
    (define val (first returnedList))
    (define ok  (second returnedList))

    (if (not ok)
        (newError (format "identifier not found: ~a" (get node Value)))
        (val)))


(define (isTruthy obj)
    (cond
        [(is-? object.NULL)   #f]
        [(is-? object.TRUE)   #t]
        [(is-? object.FALSE)  #f]
        [else                 #t]))


(define (newError formatPattern . xs)
    (new object.Error [Message (apply format formatPattern xs)])) 


(define (isError obj)
    (if (not (null? obj))
        (equal? (send obj Type) object.ERROR_OBJ)
        (#f)))

(define (evalExpressions exps env)
    (define returnValue (list)) 
    (define break-loop #f) 
    (define evaluated null)
    (for/list ([e exps]
               #:break (break-loop))
        (begin
            (set! evaluated (Eval e env))
            (if (isError evaluated)
                (begin (set! returnValue (list evaluated)) (set! breal-loop #t))
                (set! returnValue (append returnValue (list evaluated))))))
    
    returnValue)

(define (applyFunction fn args)
    (define env (object.NewEnclosedEnvironment (get fn Env)))
   
    (for/list ( [i (in-naturals 0)]
                [param (get fn Paramenters)])
        (send env Set (get param Value) (list-ref args i)))
    
    env)

(define (unwrapReturnValue obj)
    (if (is-a? obj object.ReturnValue)
        (get obj Value)
        obj))









    

          










                                                    

