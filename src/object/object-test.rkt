#lang racket

(require rackunit)
(require (prefix-in object. "../object/object.rkt"))

(define (TestStringHashKey)
  (define hello1 (new object.String [Value "Hello World"]))
  (define hello2 (new object.String [Value "Hello World"]))
  (define diff1 (new object.String [Value "My name is Johnny"]))
  (define diff2 (new object.String [Value "My name is Johnny"]))

  (check-equal? (send hello1 HashKey) (send hello2 HashKey) "same content same hashkey")
  (check-equal? (send diff1 HashKey) (send diff2 HashKey)) "same content same hashkey"
  (check-equal? (equal? (send hello1 HashKey) (send diff1 HashKey)) #f))

(define (TestBooleanHashKey)
  (define true1 (new object.Boolean [Value #t]))
  (define true2 (new object.Boolean [Value #t]))
  (define false1 (new object.Boolean [Value #f]))
  (define false2 (new object.Boolean [Value #f]))

  (check-equal? (send true1 HashKey) (send true2 HashKey))
  (check-equal? (send false1 HashKey) (send false2 HashKey))
  (check-equal? (equal? (send true1 HashKey) (send false1 HashKey)) #f))

(define (TestIntegerHashKey)
  (define one1 (new object.Integer [Value 1]))
  (define one2 (new object.Integer [Value 1]))
  (define two1 (new object.Integer [Value 2]))
  (define two2 (new object.Integer [Value 2]))

  (check-equal? (send one1 HashKey) (send one2 HashKey))
  (check-equal? (send two1 HashKey) (send two2 HashKey))
  (check-equal? (equal? (send one1 HashKey) (send two2 HashKey)) #f))

(define-syntax get
  (syntax-rules ()
    [(get obj f)
     (get-field f obj)]
    [(get obj f1 f2)
     (get (get-field f1 obj) f2)]))


(TestStringHashKey)
(TestBooleanHashKey)
(TestIntegerHashKey)
