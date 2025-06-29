#lang racket
(require dyoo-while-loop)

(require (prefix-in token. "../token/token.rkt"))

(provide (except-out (all-defined-out) isLetter isDigit))

(define Lexer
  (class object%
    (init-field input position readPosition ch)
    (super-new)
    (define/private (readChar)

        (if (>= readPosition (string-length input))
          (set! ch #\null)
          (set! ch (string-ref input readPosition)))


        (set! position readPosition)
        (set! readPosition (add1 readPosition)))
        ;; (displayln "done")



    (define/private (skipWhitespace)
        ( while (char-whitespace? ch) ;built-in function : char-whitespace?
                (readChar)))



    (define/private (peekChar)
        ( if (>= readPosition (string-length input))
             #\null
             (string-ref input readPosition)))




    (define/private (readIdentifier)
        ;; (
          (local [(define start position)]
            (while (isLetter ch) (readChar))
            (substring input start position)))

    (define/private (readNumber)
      (local [(define start position)]
        (while (isDigit ch) (readChar))
        (substring input start position)))
    
    (define/private (readString)
      (define start (+ position 1))
      (while #t (begin
                  (readChar)
                  (when (or (equal? ch #\") (equal? ch #\null)) 
                    (break))))
      (substring input start position))
      



   (define/public (NextToken)

     (define tok null)

     (skipWhitespace)

     (set! tok
           (case ch
             [ (#\=)(
                     if (char=? (peekChar) #\=)
                     ( begin
                       (readChar)
                       (newToken token.EQ "=="))

                     (newToken token.ASSIGN ch))]



             [
              (#\+) (newToken token.PLUS ch)]


             [
              (#\-) (newToken token.MINUS ch)]


             [
              (#\!) ( if (char=? (peekChar) #\=)
                         (begin
                           (readChar)
                           (newToken token.NOT_EQ "!="))

                         (newToken token.BANG ch))]
             [
              (#\/) (newToken token.SLASH ch)]

             [
              (#\*) (newToken token.ASTERISK ch)]

             [
              (#\<) (newToken token.LT ch)]

             [
              (#\>) (newToken token.GT ch)]

             [
              (#\;) (newToken token.SEMICOLON ch)]

             [
              (#\:) (newToken token.COLON ch)]

             [
              (#\,) (newToken token.COMMA ch)]

             [
              (#\{) (newToken token.LBRACE ch)]

             [
              (#\}) (newToken token.RBRACE ch)]

             [
              (#\() (newToken token.LPAREN ch)]

             [
              (#\)) (newToken token.RPAREN ch)]

             [ 
              (#\") (newToken token.STRING (readString))]

             [
              (#\[) (newToken token.LBRACKET ch)]

             [ 
              (#\]) (newToken token.RBRACKET ch)]

             [
              (#\null) (newToken token.EOF "")]

             [ else null]))




    (if (null? tok)
        (cond
          [
           (isLetter ch)   ( begin
                             (set! tok (newToken "-" (readIdentifier)))
                             (set-field! Type tok (token.LookupIdent (get-field Literal tok))))]



          [
           (isDigit ch)
           (set! tok (newToken token.INT (readNumber)))]


          [
           else   (begin
                    (set! tok (newToken token.ILLEGAL ch))
                    (readChar))])

        ( ; else branch

         readChar))


    tok) ;; END NextToken

   (define/private (initialize)
     (readChar))
      
   (initialize))) ;; END Lexer


(define (New in)
  (new Lexer [input in] [position 0] [readPosition 0] [ch #\0]))



(define (isLetter ch) ( or (char<=? #\a ch #\z) (char<=? #\A ch #\Z)))



(define (isDigit ch) ( char<=? #\0 ch #\9))


(define (newToken tokenType ch)  (
                                  new token.Token [Type tokenType] [Literal (~a ch)]))
