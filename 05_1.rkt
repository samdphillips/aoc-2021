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

(define (line-axis-aligned? a-line)
  (match a-line
    [(line (posn x _) (posn x _)) #t]
    [(line (posn _ y) (posn _ y)) #t]
    [_ #f]))

(define (in-line-points a-line)
  (define (make-range a b)
    (if (< b a)
        (make-range b a)
        (in-range a (add1 b))))
  (define-values (range make-posn)
    (match a-line
      [(line (posn x y0) (posn x y1))
       (values (make-range y0 y1)
               (lambda (y) (posn x y)))]
      [(line (posn x0 y) (posn x1 y))
       (values (make-range x0 x1)
               (lambda (x) (posn x y)))]))
  (in-generator
   (for ([c range]) (yield (make-posn c)))))   

(define (lines->map a-line-list)
  (for*/fold ([m (hash)]) ([a-line (in-list a-line-list)]
                           [a-posn (in-line-points a-line)])
    (hash-update m a-posn add1 0)))

(struct posn (x y) #:transparent)

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
    (sequence-filter line-axis-aligned? _)
    sequence->list
    lines->map
    in-hash-values
    (sequence-filter (lambda~> (> 1)) _)
    sequence-length)