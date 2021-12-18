#lang racket

(require advent-of-code
         graph
         racket/generator
         threading)

(define test-input #<<```
start-A
start-b
A-c
A-b
b-d
A-end
b-end
```
  )

(define (read-edge inp)
  (match (read-line inp)
    [(? eof-object?) eof]
    [(regexp #px"^([^-]+)-([^-]+)$" (list _ src dest)) (list src dest)]))

(define (small-cave? s)
  (char-lower-case? (string-ref s 0)))

(define (visited? vs v) (member v vs))

;; find all paths from "start" to "end" in g
(define (find-paths g)
  (in-generator
   (define (explore src path)
     (cond
       [(string=? src "end") (yield (cons src path))]
       [else
        (for ([n (in-neighbors g src)]
              #:unless (and (small-cave? n) (visited? path n)))
          (explore n (cons src path)))]))
   (explore "start" null)))

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 12 #:cache #t)
    (port->list read-edge _)
    unweighted-graph/undirected
    find-paths
    sequence-length)