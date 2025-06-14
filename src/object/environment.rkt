#lang racket

(require threading) ; for ~>

(define (NewEnclosedEnvironment outer)
    (define env (NewEnvironment))
    (~> env (set-field! outer _ outer))
    env)


(define (NewEnvironment)
    (define s (make-hash))
    (new Environment [store s] [outer null]))


(define Environment
    (class object%
              (super-new)
              (init-field store outer)
              (define/public (Get name)
                    (define obj (hash-ref store name #f))
                    (define ok (eq? obj #f))
                    (when (and (not ok) (not (null? outer)))
                        (begin
                            (define ret (send outer Get name))
                            (set! obj (first ret))
                            (set! ok  (second ret))))
                    (list obj ok)

                (define/public (Set name val)
                    (hash-set! store name val)
                    val))))
