#lang racket

(require (prefix-in ast. "../ast/ast.rkt"))


(define NULL_OBJ   "NULL")
(define ERROR_OBJ  "ERROR")
(define INTEGER_OBJ  "INTEGER")
(define BOOLEAN_OBJ  "BOOLEAN")
(define RETURN_VALUE_OBJ  "RETURN_VALUE")
(define FUNCTION_OBJ  "FUNCTION")

(define Object
  (class object%
    (super-new)
    (define/public (Type) null)
    (define/public (Inspect) null)))

(define Integer
  (class Object
    (super-new)
    (init-field Value)
    (define/override (Type) INTEGER_OBJ)
    (define/override (Inspect) (format "~a" Value))))

(define Boolean
  (class Object
    (super-new)
    (init-field Value)
    (define/override (Type) BOOLEAN_OBJ)
    (define/override (Inspect) (format "~a" Value))))

(define Null
  (class Object
    (super-new)
    (init-field Value)
    (define/override (Type) NULL_OBJ)
    (define/override (Inspect) "null")))


(define ReturnValue
  (class Object
    (super-new)
    (init-field Value)
    (define/override (Type) RETURN_VALUE_OBJ)
    (define/override (Inspect) (send Value Inspect))))


(define Error
  (class Object
    (super-new)
    (init-field Message)
    (define/override (Type) ERROR_OBJ)
    (define/override (Inspect) (format "ERROR: ~a" Message))))


(define Function
  (class Object
    (super-new)
    (init-field Parameters Body Env)
    (define/override (Type) FUNCTION_OBJ)
    (define/override (Inspect) (begin
                                 (define params (map (lambda (e) (send e String)) Parameters))
                                 (string-append
                                   "fn"
                                   "("
                                   (string-join params ", ")
                                   ") {\n"
                                   (send Body String)
                                   "\n}")))))
