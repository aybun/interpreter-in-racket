#lang racket

(require threading) ; for ~>

(provide NewEnclosedEnvironment NewEnvironment Environment)


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
                  (printf "in Environment.Get\n") 
                  (define obj (hash-ref store name 'does-not-exist))
                  (define ok (not (equal? obj 'does-not-exist)))
                  (when (and (not ok) (not (null? outer)))
                      (begin
                          (define ret (send outer Get name))
                          (set! obj (first ret))
                          (set! ok  (second ret))))
                  (printf "obj: ~a\n" obj)
                  (printf "ok: ~a\n" ok)
                  (list obj ok))

            (define/public (Set name val)
                (hash-set! store name val)
                val)))
