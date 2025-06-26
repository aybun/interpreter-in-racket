#lang racket

(require (prefix-in object. "../object/object.rkt"))
(require (prefix-in evaluator. "../evaluator/evaluator.rkt"))

(define builtins 
  (hash 
    "len" (new object.Builtin 
              [Fn (lambda args
                      
                    (cond 
                        [(not (equal? (length args) 1)) (evaluator.newError "wrong number of arguments. got~a, want=1" (length args))]
                        [(is? (first args) object.Array) (new object.Integer [Value (length (get (first args) Elements))])]
                        [(is? (first args) object.String) (new object.Integer [Value (length (get (first args) Value))])]
                        [else (evaluator.newError "argument to `len` not supported, got ~a" (send (first args) Type))]))])
    "puts" (new object.Builtin
              [Fn (lambda args
                    (begin
                        (for/list (arg args)
                          (printf "~a\n" (send arg Inspect)))
                        null))])
    "first" (new object.Builtin 
              [Fn (lambda args
                    (cond
                        [(not (equal? (length args) 1)) (evaluator.newError "wrong number of arguments. got=~a, want=1" (length args))]
                        [(not (equal? (send (first args) Type) object.ARRAY_OBJ)) (evaluator.newError "argument to `first` must be ARRAY, got ~a" (send (first args) Type))]
                        [else (begin
                                (define arr (first args))
                                (define arr-length (length arr))
                                (if (> arr-length 0)
                                    (first (get arr Elements))
                                    null))]))])
    "last" (new object.Builtin 
              [Fn (lambda args
                    (cond
                        [(not (equal? (length args) 1)) (evaluator.newError "wrong number of arguments. got=~a, want=1" (length args))]
                        [(not (equal? (send (first args) Type) object.ARRAY_OBJ)) (evaluator.newError "argument to `last` must be ARRAY, got ~a" (send (first args) Type))]
                        [else (begin
                                (define arr (first args))
                                (define arr-length (length arr))
                                (if (> arr-length 0)
                                    (last (get arr Elements))
                                    null))]))])

    "rest" (new object.Builtin 
              [Fn (lambda args
                    (cond
                        [(not (equal? (length args) 1)) (evaluator.newError "wrong number of arguments. got=~a, want=1" (length args))]
                        [(not (equal? (send (first args) Type) object.ARRAY_OBJ)) (evaluator.newError "argument to `rest` must be ARRAY, got ~a" (send (first args) Type))]
                        [else (begin
                                (define arr (first args))
                                (define arr-length (length arr))
                                (if (> arr-length 0)
                                    (new object.Array [Elements rest (get arr Elements)])
                                    null))]))])
    "push" (new object.Builtin
              [Fn (lambda args
                    (cond
                        [(not (equal? (length args) 2)) (evaluator.newError "wrong number of arguments. got=~a, want=2" (length args))]
                        [(not (equal? (send (first args) Type) object.ARRAY_OBJ)) (evaluator.newError "argument to `push` must be ARRAY, got ~a" (send (first args) Type))]
                        [else (begin
                                (define arr (first args))
                                (define arr-length (length arr))
                                (new object.Array [Elements (append (get arr Elements) (second args))]))]))])))

; Macros
(define-syntax get
  (syntax-rules ()
    [(get obj f)
     (get-field f obj)]
    [(get obj f1 f2)
     (get (get-field f1 obj) f2)]))
