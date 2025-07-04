#lang racket
(require racket/system)

(require (prefix-in repl. "repl/repl.rkt"))

(define (Main)
  (define user (or (getenv "USER") (getenv "USERNAME")))
  (unless user
    (error "Could not get current user"))
  (printf "Hello ~a! This is the Monkey programming language!\n" user)
  (printf "Feel free to type in commands\n")
  (repl.Start (current-input-port) (current-output-port)))

(Main)
