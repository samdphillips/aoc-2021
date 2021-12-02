#lang racket

(require (for-syntax racket/syntax)
         advent-of-code
         syntax/parse/define
         threading)

(struct posn (x y) #:transparent)

(define-syntax-parse-rule (mod-func field op)
  #:with ref (format-id #'field "posn-~a" #'field)
  (lambda (amt)
    (lambda (p)
      (struct-copy posn p [field (op (ref p) amt)]))))

(define dirfuncs
  (hash 'forward (mod-func x +)
        'up      (mod-func y -)
        'down    (mod-func y +)))
        
(define (read-command p)
  (match (read-line p)
    [(? eof-object? e) e]
    [(regexp #px"^(forward|down|up)\\s+(\\d+)$"
             (list _
                   (app string->symbol dir)
                   (app string->number amt)))
     ((hash-ref dirfuncs dir) amt)]))

(define (debug v) #;(displayln v) v)

(define (eval-commands cmds)
  (for/fold ([p (posn 0 0)]) ([cmd cmds])
    (debug (cmd p))))

(~> (open-aoc-input (find-session) 2021 2 #:cache #t)                       
    (in-port read-command _)
    eval-commands)
