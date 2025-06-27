#lang racket

(provide (all-defined-out))

(define Node
  (class object%
    (super-new)

    (define/public (TokenLiteral) null)
    (define/public (String) null)))



(define Statement
  (class Node
    (define/public (statementNode) null)

    (super-new)))

(define Expression
  (class Node

    (super-new)
    (define/public (expressionNode) null)))




(define Program

  (class object%
    (super-new)
    (init-field Statements)
    (define/public (TokenLiteral) (if (> (length Statements ) 0)
                                      (send (first Statements) TokenLiteral)
                                      ""))

    (define/public (String) (string-append* (map (lambda (e) (send e String)) Statements)))))





;;Statements

(define LetStatement
  (class Statement
    (super-new)
    (init-field Token Name Value)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (string-append
                                (TokenLiteral) " "
                                (send Name String)
                                " = "
                                (if (null? Value) "" (send Value String))
                                ";"))))





(define ReturnStatement
  (class Statement
    (super-new)
    (init-field Token ReturnValue)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (string-append
                                (TokenLiteral) " "
                                (if (null? ReturnValue) "" (send ReturnValue String))
                                ";"))))






(define ExpressionStatement
  (class Statement
    (super-new)
    (init-field Token Expression)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (string-append
                                (if (null? Expression) "" (send Expression String))))))






(define BlockStatement
  (class Statement
    (super-new)
    (init-field Token Statements)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (string-append*  (map (lambda (e) (send e String)) Statements)))))




;; Expressions


(define Identifier
  (class Expression
    (super-new)
    (init-field Token Value)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) Value)))



(define Boolean
  (class Expression
    (super-new)
    (init-field Token Value)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (get-field Literal Token))))




(define IntegerLiteral
  (class Expression
    (super-new)
    (init-field Token Value)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (get-field Literal Token))))




(define PrefixExpression
  (class Expression
    (super-new)
    (init-field Token Operator Right)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (string-append
                               "("
                               Operator
                               (send Right String)
                               ")"))))







(define InfixExpression
  (class Expression
    (super-new)
    (init-field Token Left Operator Right)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (string-append
                               "("
                               (send Left String)
                               " " Operator " "
                               (send Right String)
                               ")"))))






(define IfExpression
  (class Expression
    (super-new)
    (init-field Token Condition Consequence Alternative)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (string-append
                               "if"
                               (send Condition String)
                               " "
                               (send Consequence String)
                               ")"

                               (when (null? Alternative) (string-append "else " (send Alternative String)))))))








(define FunctionLiteral
  (class Expression
    (super-new)
    (init-field Token Parameters Body)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (begin
                                (define params (map (lambda (e) (send e String)) Parameters))
                                (define joined-strings (string-join params ", "))

                                (string-append (TokenLiteral)
                                               "("
                                               joined-strings
                                               ")"
                                               (send Body String))))))








(define CallExpression
  (class Expression
    (super-new)
    (init-field Token Function Arguments)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (begin
                                (define args (map (lambda (e) (send e String)) Arguments))
                                (define joined-strings (string-join args ", "))

                                (string-append (send Function String)
                                               "("
                                               joined-strings
                                               ")")))))

(define StringLiteral
  (class Expression
    (super-new)
    (init-field Token Value)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (get-field Literal Token))))



(define ArrayLiteral
  (class Expression
    (super-new)
    (init-field Token Elements)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String)
      (define elements (map (lambda (e) (send e String)) Elements))
      (string-append "["
                     (string-join elements ", ")
                     "]"))))

(define IndexExpression
  (class Expression
    (super-new)
    (init-field Token Left Index)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String) (string-append "("
                                             (send Left String)
                                             "["
                                             (send Index String)
                                             "])"))))

(define HashLiteral
  (class Expression
    (super-new)
    (init-field Token Pairs)
    (define/override (TokenLiteral) (get-field Literal Token))
    (define/override (String)
      (define pairs (list))
      (hash-for-each Pairs
                     (lambda (key value)
                       (append pairs (list (string-append (send key String) ":" (send value String))))))
      (string-append "{"
                     (string-join pairs ", ")
                     "}"))))
