#lang racket
(require dyoo-while-loop)

(require (prefix-in token. "../token/token.rkt"))

(provide (except-out (all-defined-out) isLetter isDigit))

(define Lexer
  (class object%
    (init-field input position readPosition ch tok)
    (super-new)
    (define/private (readChar)

        (if (>= readPosition (string-length input) )
          (set! ch #\null)
          (set! ch (string-ref input readPosition))
        )

        (set! position readPosition)
        (set! readPosition (add1 readPosition))
        ;; (displayln "done")
    )


    (define/private (skipWhitespace)
        ( while (char-whitespace? ch) ;built-in function : char-whitespace?
                (readChar)
        )
    )

    (define/private (peekChar)
        ( if (>= readPosition (string-length input))
             #\null
             (string-ref input readPosition)
        )
    )


    (define/private (readIdentifier)
        ;; (
          (local [(define start position)]
            (while (isLetter ch) (readChar) )
            (substring input start position)
          )
            
        ;; )
    )


    (define/private (readNumber)
        ;; (
          (local [(define start position)]
            (while (isDigit ch) (readChar) )
            (substring input start position)
          )
        ;; )
    )


  (define/public (NextToken)
      ;; (

        (skipWhitespace)

        (set! tok
          (case ch
          [ (#\=)(
                   if (char=? (peekChar) #\=)
                     ( begin
                       #| (define temp-ch ch) |#
                      ;; (displayln peek)
                       (readChar)
                       #| (define literal (string temp-ch ch)) |#
                       (newToken token.EQ "==")
                      )
                     (
                      newToken token.ASSIGN ch
                      )
                 )
          ]
          [
            (#\+) (newToken token.PLUS ch )

          ]
          [
            (#\-) (newToken token.MINUS ch)

          ]
          [
            (#\!) ( if (char=? (peekChar) #\=)
                     (begin
                        #| (define temp-ch ch) |#
                        (readChar)
                        #| (define literal (string temp-ch ch)) |#
                        (newToken token.NOT_EQ "!=")
                      )
                     (
                        newToken token.BANG ch
                      )

                 )

          ]
          [
            (#\/) (newToken token.SLASH ch)
          ]
          [
            (#\*) (newToken token.ASTERISK ch)
          ]
          [
            (#\<) (newToken token.LT ch)
          ]
          [
            (#\>) (newToken token.GT ch)
          ]
          [
            (#\;) (newToken token.SEMICOLON ch)
          ]
          [
            (#\,) (newToken token.COMMA ch)
          ]
          [
            (#\{) (newToken token.LBRACE ch)
          ]
          [
            (#\}) (newToken token.RBRACE ch)
          ]
          [
            (#\() (newToken token.LPAREN ch)
          ]
          [
            (#\)) (newToken token.RPAREN ch)
          ]
          [
            (#\null) (newToken token.EOF "")
          ]
          [ else null
          ]
          )
        )

        (if (null? tok)
          (
              cond
              [
                (isLetter ch)   ( begin
                                  (set! tok (newToken "-" (readIdentifier)) )
                                  (set-field! Type tok (token.LookupIdent (get-field Literal tok) ) )
                                )

              ]
              [
               (isDigit ch)
                               (set! tok (newToken token.INT (readNumber) ) )

              ]
              [
               else   (begin
                        (set! tok (newToken token.ILLEGAL ch))
                        (readChar)
                       )

              ]
          )
          ( ;non-null branch

             readChar
          )
        )
        tok
      ;; )
  )
    ;; About to end.
  (define/private (initialize)
    (readChar)

  )
  (initialize)
))

(define (New in)
  (new Lexer [input in] [position 0] [readPosition 0] [ch #\0] [tok "tok"])
  ;; (printf "in New : ~a\n" l)
  ;;(send l readChar)
  ;;(get-field position l)
  ;; (displayln "After readChar")
  ;; (displayln l)
  ;;l
)


(define (isLetter ch) ( or (char<=? #\a ch #\z) (char<=? #\A ch #\Z ) 
                      )
)

(define (isDigit ch) ( char<=? #\0 ch #\9
                      )
)
; Why is newToken here?
; Notice that n is in lowercase.
; That means it is meant to be called inside this package.
; (~a ch) means casting ch to string.
(define (newToken tokenType ch)  (
                                  new token.Token [Type tokenType] [Literal (~a ch)]
                                  )
)

