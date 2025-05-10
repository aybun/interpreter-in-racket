#lang racket
(require dyoo-while-loop)

(require (prefix-in token. "../token/token.rkt"))

; Define new interface to support named argument.
(define Lexer
  (class object%
    (init-field input position readPosition ch tok)
    
    (define/public (readChar)
      (

        (if (>= position (string-length input) ) 
          (set! ch #\0)
          (set! ch (string-ref input readPosition))
        )

        (set! position readPosition)
        (set! readPosition (add1 readPosition))

      ) 
    )


    (define/public (skipWhitespace)
        ( while (member ch (list #\space #\t #\n #\r))
                ;Note, just to repeat.
                ;This is a function call with 0 arguments.
                (readChar)
        )
    )

    (define/public (peekChar)
        ( if (>= readPosition (string-length input))
             #\0
             (string-ref input readPosition)
        )
    )


    (define/public (readIdentifier)
        (
          (local [(define start position)]
            (while (isLetter ch) (readChar) )
            (substring input start position)
          )
            
        )
    )


    (define/public (readNumber)
        (
          (local [(define start position)]
            (while (isDigit ch) (readChar) )
            (substring input start position)
          )
        )
    )


  (define/public (NextToken)
      (

        (skipWhitespace)
        
        (set! tok 
          (case ch
          [ (#\=)( 
                   if (= (peekChar) #\=)
                     (
                       #| (define temp-ch ch) |#
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
            (#\!) ( if (= (peekChar) #\=)
                     (
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
            (#\0) (newToken token.EOF "")
          ]
          [ else null
          ]
          )
        )
        
        (if (null? tok)
          (
              cond   
              [
                (isLetter ch)   (
                                  (set! tok (newToken "-" (readIdentifier)) )
                                  (set-field! Type tok (token.LookupIdent (get-field Literal tok) ) )

                                )
               
              ]
              [
                (isDigit ch) (
                               (set! tok (newToken token.INT (readNumber) ) )
                              )

              ]
              [
                else ( 
                       (set! tok (newToken token.ILLEGAL ch)) 
                       (readChar)

                     )

              ]
          )
          ( ;non-null branch

            (readChar)
          )
        )
        tok
      )
  )
    ;; About to end.

    (super-new)))

(define (New in)
  (define l (new Lexer [input in] [position 0] [readPosition 0] [ch #\0] [tok "tok"]) )
  (send l readChar)
  l
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
(define (newToken tokenType ch)  (
                                     token.Token (tokenType (string ch) )
                                  )
)

