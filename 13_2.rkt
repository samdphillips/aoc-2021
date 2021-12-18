#lang racket

(require advent-of-code
         pict
         racket/draw
         syntax/parse/define
         threading
         "13_1.rkt")

(define (dots some-dots)  
  (define-values (xo yo w h)
    (for/fold ([x0 #f]
               [x1 #f]
               [y0 #f]
               [y1 #f]
               #:result (values x0 y0 (add1 (- x1 x0)) (add1 (- y1 y0))))
              ([a-posn some-dots])
      (define-syntax-parse-rule (next f:id v0:id v1:id) (if v0 (f v0 v1) v1))
      (match-define (posn x y) a-posn)
      (values (next min x0 x) (next max x1 x)
              (next min y0 y) (next max y1 y))))
  (freeze
   (dc (lambda (a-dc dx dy)
         (define old-pen (send a-dc get-pen))
         (define old-brush (send a-dc get-brush))
         (send a-dc set-pen (make-pen #:color "black"))
         (send a-dc set-brush (make-brush #:color "black"))
         (send a-dc draw-rectangle dx dy w h)
         (send a-dc set-pen
               (make-pen #:color "green"
                         #:cap 'projecting
                         #:join 'miter
                         #:width 1))
         (for ([a-posn some-dots])
           (define-syntax-parse-rule (tx acc:id p:id o:id d:id)
             (+ d (- (acc p) o)))
           (send a-dc draw-point
                 (tx posn-x a-posn xo dx)
                 (tx posn-y a-posn yo dy)))
         (send a-dc set-pen old-pen)
         (send a-dc set-brush old-brush))
       w h)))

(define puzzle-complete?
  (lambda~> puzzle-folds null?))

(define (puzzle-run p)
  (cond
    [(puzzle-complete? p) p]
    [else
     (puzzle-run (fold-step p))]))

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 13 #:cache #t)
    read-puzzle
    puzzle-run
    puzzle-dots
    dots
    (scale _ 10 10))