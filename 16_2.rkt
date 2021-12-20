#lang racket

(require advent-of-code
         threading
         "16_1.rkt")

(define (cmp/op f)
  (lambda (a b)
    (if (f a b) 1 0)))

(define (apply-op op pkt*)
  (define opf
    (match op
      [0 +]
      [1 *]
      [2 min]
      [3 max]
      [5 (cmp/op >)]
      [6 (cmp/op <)]
      [7 (cmp/op =)]))
  (apply opf (map evaluate pkt*)))

(define (evaluate pkt)
  (match pkt
    [($literal _ v) v]
    [($operator _ op pkt*)
     (apply-op op pkt*)]))

(define (evaluate/string str)
  (~> str decode evaluate))

(module* main #f
  (~> (open-aoc-input (find-session) 2021 16 #:cache #t)
      port->string
      evaluate/string))
