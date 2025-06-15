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
      [(is-a? ast.Program) (evalProgram node env)]  
      [(is-a? ast.BlockStatement) (evalBlockStatement node env)]
      [(is-a? ast.ExpressionStatement) (Eval (get node Expression) env)]
      [(is-a? ast.ReturnStatement) (begin
                                      (define val (Eval (get node ReturnValue) env))
                                      (if (isError val) 
                                          val
                                          (new object.ReturnValue [Value val])))]
      [(is-a? ast.LetStatement) (begin
                                    (define val (get node Value))
                                    (if (isError val)
                                        val
                                        (begin 
                                            (send env Set (get node Name Value) val)
                                            null)))]
      [(is-a? ast.IntegerLiteral) (new object.Integer [Value (get node Value)])]
      [(is-a? ast.Boolean) (nativeBoolToBooleanObject (get node Value))]
      [(is-a? ast.PrefixExpression) (begin
                                        (define right (Eval (get node Right)) env)
                                        (if (isError right)
                                            right
                                            (evalPrefixExpression (get node Operator) right)))]
      [(is-a? ast.InfixExpression) (begin
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
      [(is-a? ast.IfExpression) (evalIfExpression node env)]
      [(is-a? ast.Identifier (evalIdentifier node env))]
      [(is-a? ast.FunctionLiteral (new object.Function [Parameters (get node Paramenters)] [Env env] [Body (get node Body)]))]
      [(is-a? ast.CallExpression) (begin
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

                                                    

