#lang racket

(define traceLevel 0)

(define traceIdentPlaceholder "\t")

(define (identLevel)
  (apply string-append (make-list (- trace-level 1) trace-ident-placeholder)))

(define (tracePrint fs)
  (printf "~a~a\n" (identLevel) fs))

(define (incIdent) (set! traceLevel (+ traceLevel 1)))
(define (decIdent) (set! traceLevel (- traceLevel 1)))

(define trace(msg)
  (incIdent)
  (tracePrint (string-append "BEGIN " msg))
  msg)

(define (untrace msg)
  (tracePrint (string-append "END " msg))
  (decIdent))
  
