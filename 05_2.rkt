#lang racket

(require (for-syntax syntax/parse)
         advent-of-code
         racket/generator
         threading)

(define test-input #<<DATA
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
DATA
  )

(struct line (src dest) #:transparent)

(define (dx-dy a-line)
  (define (sgn2 a b) (sgn (- b a)))
    (match a-line
      [(line (posn x y0) (posn x y1))
       (values 0 (sgn2 y0 y1))]
      [(line (posn x0 y) (posn x1 y))
       (values (sgn2 x0 x1) 0)]
      [(line (posn x0 y0) (posn x1 y1))
       (values (sgn2 x0 x1)
               (sgn2 y0 y1))]))

(define (in-line-points a-line)
  (define-values (dx dy) (dx-dy a-line))
  (define end-posn (line-dest a-line))
  (define (points a-posn yield)
    (yield a-posn)
    (unless (equal? a-posn end-posn)
      (points (posn+ a-posn dx dy) yield)))
  (in-generator (points (line-src a-line) yield)))

(define (lines->map a-line-seq)
  (for*/fold ([m (hash)]) ([a-line a-line-seq]
                           [a-posn (in-line-points a-line)])
    (hash-update m a-posn add1 0)))

(struct posn (x y) #:transparent)

(define (posn+ a-posn dx dy)
  (struct-copy posn a-posn
               [x (+ dx (posn-x a-posn))]
               [y (+ dy (posn-y a-posn))]))

(define-match-expander num
  (syntax-parser
    [(_ pat) #'(? string? (app string->number (? number? pat)))]))

(define (read-input-line inp)
  (match (read-line inp)
    [(? eof-object? eof) eof]
    [(regexp #px"^(\\d+),(\\d+)\\s+->\\s+(\\d+),(\\d+)$"
             (list _ (num x0) (num y0) (num x1) (num y1)))
     (line (posn x0 y0) (posn x1 y1))]
    [input (error 'read-input-line "malformed input: ~s" input)]))

(define (dbg x)
  (displayln "-=-=-=-=-") (pretty-print x) (displayln "-=-=-=-=-") x)

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 5 #:cache #t)
    (in-port read-input-line _)
    lines->map
    in-hash-values
    (sequence-filter (lambda~> (> 1)) _)
    sequence-length)