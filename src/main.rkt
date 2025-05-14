#lang racket

(require (prefix-in token. "./token/token.rkt"))
(require racket/class)


(define (distance)
  (77)
  )

(define Point
  (class object%
    (init-field x y)
    (define/public (distance)
      (sqrt (+ (sqr x) (sqr y))))
    (define/public (double-distance)
      (* 2 (distance) ) )
    (define/public (reset)
      (set! x 0) (set! y 0) )
    (super-new)))

(define p (new Point [x 3] [y 4]))
(send p distance)
(send p double-distance)
(send p reset)
(get-field x p)
(get-field y p)


(displayln "new exp:")


(define keywords (hasheq
                        "fn" "FUNCTION"
                        "let" "LET"
                         ))
(define tok (hash-ref keywords "fn" null))
(printf "tok: ~a\n" tok)


(printf "~a\n" (= #\z #\=))


