#lang racket

(require advent-of-code
         threading)

(provide (all-defined-out))

(define test-input #<<```
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
```
  )

(struct posn (x y) #:transparent)

(struct fold (axis pos) #:transparent)

(struct puzzle (dots folds) #:transparent)

(define puzzle-dots-count
  (lambda~> puzzle-dots
            set-count))

(define-match-expander num
  (syntax-rules ()
    [(_ pat) (app string->number (? number? pat))]))

(define-match-expander sym
  (syntax-rules ()
    [(_ pat) (app string->symbol pat)]))

(define (read-point inp)
  (match (read-line inp)
    [(regexp #px"^\\s*$") eof]
    [(regexp #px"^(\\d+),(\\d+)$" (list _ (num x) (num y)))
     (posn x y)]))

(define (read-fold inp)
  (match (read-line inp)
    [(? eof-object?) eof]
    [(regexp #px"^fold along (x|y)=(\\d+)$"
             (list _ (sym axis) (num pos)))
     (fold axis pos)]))

(define (read-puzzle inp)
  (define dots
    (for/set ([d (in-port read-point inp)]) d))
  (define folds (port->list read-fold inp))
  (puzzle dots folds))

(define (fold-at p v)
  (cond
    [(< v p) v]
    [(> v p) (- (* 2 p) v)]
    [else
     (error 'fold-at "value is on the fold: ~a" v)]))

(define (fold-step p)
  (match-define (puzzle dots (cons cur-fold next-folds)) p)
  (define fold-dot
    (match cur-fold
      [(fold 'x v) (lambda (a-posn)
                     (posn (fold-at v (posn-x a-posn))
                           (posn-y a-posn)))]
      [(fold 'y v) (lambda (a-posn)
                     (posn (posn-x a-posn)
                           (fold-at v (posn-y a-posn))))]))    
  (puzzle (for/set ([a-dot (in-set dots)]) (fold-dot a-dot))
          next-folds))

(module* main #f
  (~> #;(open-input-string test-input)
      (open-aoc-input (find-session) 2021 13 #:cache #t)
      read-puzzle
      fold-step
      puzzle-dots-count))
