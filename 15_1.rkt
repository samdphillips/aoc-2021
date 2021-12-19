#lang racket

(require advent-of-code
         data/gen-queue/priority
         graph
         threading)

(define test-input #<<```
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
```
  )

(struct puzzle (cols rows cells)
  #:transparent
  #:methods gen:graph
  [(define (in-vertices a-puzzle)
     (for*/stream ([y (in-range (puzzle-rows a-puzzle))]
                   [x (in-range (puzzle-cols a-puzzle))])
       (cons x y)))
   
   (define (in-neighbors a-puzzle posn)
     (match-define (cons x y) posn)
     (for*/list ([d (in-list '((0 . -1) (0 . 1) (-1 . 0) (1 . 0)))]
                 [dx (in-value (car d))]
                 [dy (in-value (cdr d))]
                 [nx (in-value (+ x dx))]
                 [ny (in-value (+ y dy))]
                 #:when (in-bounds? a-puzzle nx ny))
       (cons nx ny)))

   (define (edge-weight a-puzzle u v #:default [default +inf.0])
     (match-define (cons x y) v)
     (match-define (puzzle _ c s) a-puzzle)
     (define i (xy->index c x y))
     (vector-ref s i))
   ])

(define (in-bounds? a-puzzle x y)
  (match-define (puzzle cols rows _) a-puzzle)
  (and (<= 0 x) (<= 0 y)
       (< x cols) (< y rows)))

(define (xy->index c x y) (+ x (* y c)))
(define (index->xy c i)
  (define-values (y x) (quotient/remainder i c))
  (values x y))

(define (read-puzzle inp)
  (define raw (port->lines inp))
  (define cols (string-length (car raw)))
  (define rows (length raw))
  (define cells (make-vector (* rows cols) #f))
  (define (char->num c)
    (- (char->integer c) (char->integer #\0)))

  (for ([y (in-naturals)]
        [row (in-list raw)])
    (for ([x (in-naturals)]
          [c (in-string row)])
      (vector-set! cells
                   (xy->index cols x y)
                   (char->num c))))
  (puzzle cols rows cells))

;; Crib the guts from the algorithm in the graph library.  It doesn't work
;; with the generic interface :(
(define (dijkstra G s)
  (define-vertex-property G d #:init +inf.0)
  (define-vertex-property G π #:init #f)
  (define w (λ (u v) (edge-weight G u v)))  
  (do-bfs G s
          #:init-queue: (mk-empty-priority (λ (u v) (< (d u) (d v))))
          #:init: (d-set! s 0)
          #:enqueue?: (> (d $v) (+ (d $from) (w $from $v)))
          #:on-enqueue: 
          (d-set! $v (+ (d $from) (w $from $v)))
          (π-set! $v $from)
          #:return: (values (d->hash) (π->hash))))

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 15 #:cache #t)
    read-puzzle
    ((lambda (p)
       (define-values (total-weight paths) (dijkstra p '(0 . 0)))
       (hash-ref total-weight (cons (sub1 (puzzle-cols p))
                                    (sub1 (puzzle-rows p)))))))
