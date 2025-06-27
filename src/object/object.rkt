#lang racket

(require (prefix-in ast. "../ast/ast.rkt"))

(provide (all-defined-out))

(define NULL_OBJ   "NULL")
(define ERROR_OBJ  "ERROR")
(define INTEGER_OBJ  "INTEGER")
(define BOOLEAN_OBJ  "BOOLEAN")
(define STRING_OBJ "STRING")
(define RETURN_VALUE_OBJ  "RETURN_VALUE")
(define FUNCTION_OBJ  "FUNCTION")
(define BUILTIN_OBJ "BUILTIN")
(define ARRAY_OBJ "ARRAY")
(define HASH_OBJ "HASH")

(struct HashKey (Type Value) #:transparent #:constructor-name -hash-key)

(define Hashable (interface () HashKey))
(define Object
  (class object%
    (super-new)
    (define/public (Type) null)
    (define/public (Inspect) null)))

(define Integer
  (class* Object (Hashable)
    (super-new)
    (init-field Value)
    (define/override (Type) INTEGER_OBJ)
    (define/override (Inspect) (format "~a" Value))
    (define/public (HashKey)
      (-hash-key (Type) Value))))

(define Boolean
  (class* Object (Hashable)
    (super-new)
    (init-field Value)
    (define/override (Type) BOOLEAN_OBJ)
    (define/override (Inspect) (format "~a" Value))
    (define/public (HashKey)
      (-hash-key (Type) (cond
                             [Value 1]
                             [else  0])))))



(define Null
  (class Object
    (super-new)
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

(define String
  (class* Object (Hashable)
    (super-new)
    (init-field Value)
    (define/override (Type) STRING_OBJ)
    (define/override (Inspect) Value)
    (define/public (HashKey) (-hash-key (Type) (equal-hash-code Value)))))

(define Builtin
  (class Object
    (super-new)
    (init-field Fn)
    (define/override (Type) BUILTIN_OBJ)
    (define/override (Inspect) "builtin function")))


(define Array
  (class Object
    (super-new)
    (init-field Elements)
    (define/override (Type) ARRAY_OBJ)
    (define/override (Inspect)
      (define elements (map (lambda (e) (send e Inspect)) Elements))
      (string-append
        "["
        (string-join elements ", ")
        "]"))))


(define HashPair
  (class object%
    (super-new)
    (init-field Key Value)))

(define Hash
  (class Object
    (super-new)
    (init-field Pairs)
    (define/override (Type) HASH_OBJ)
    (define/override (Inspect)
      (define pairs (map (lambda (e)
                           (format "~a: ~a" (send (get e Key) Inspect) (send (get e Value) Inspect)))))
      (string-append
        "{"
        (string-join pairs ", ")
        "}"))))



; Macros
(define-syntax get
  (syntax-rules ()
    [(get obj f)
     (get-field f obj)]
    [(get obj f1 f2)
     (get (get-field f1 obj) f2)]))
