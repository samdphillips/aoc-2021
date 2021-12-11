#lang racket

(require advent-of-code
         threading)

(define test-input #<<DATA
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
DATA
  )

(define (xy->index x y) (+ x (* y 10)))
(define (index->xy i)
  (define-values (y x) (quotient/remainder i 10))
  (values x y))

(define (in-bounds? i) (<= 0 i 9))

(define (neighbors i)
  (define-values (x y) (index->xy i))
  (define (not-self? nx ny)
    (not (and (= x nx) (= y ny))))  
  (for*/list ([ny (in-range (sub1 y) (+ 2 y))]
              [nx (in-range (sub1 x) (+ 2 x))]
              #:when (and (not-self? nx ny)
                          (in-bounds? nx)
                          (in-bounds? ny)))
    (xy->index nx ny)))

(define (load-map inp)
  (for/vector #:length 100 ([ch (in-port read-char inp)]
                            #:when (char-numeric? ch))
    (- (char->integer ch) (char->integer #\0))))

(define (should-flash? m i)
  (< 9 (vector-ref m i)))

(define (incr-cell! m i pending)
  (vector-set! m i (add1 (vector-ref m i)))
  (if (should-flash? m i)
      (cons i pending)
      pending))

(define (reset-cells! m flashed)
  (for ([i (in-set flashed)])
    (vector-set! m i 0)))

(define (incr-all! m)
  (for/fold ([pending null]) ([i (in-range 100)])
    (incr-cell! m i pending)))

(define (flash-cell! m i)  
  (define (do-flash)
    (for/fold ([pending null]) ([j (in-list (neighbors i))])
      (incr-cell! m j pending)))
  (cond
    [(should-flash? m i) (do-flash)]
    [else null]))

(define (process-flashes! m pending seen)
  (cond
    [(stream-empty? pending) seen]
    [else
     (define i (stream-first pending))
     (define-values (more-pending new-seen)
       (cond
         [(set-member? seen i)
          (values null seen)]
         [else
          (values (flash-cell! m i) (set-add seen i))]))         
     (process-flashes! m
                       (stream-append (stream-rest pending)
                                      more-pending)
                       new-seen)]))

(define (display-map m)
  (for ([j (in-range 10)])
    (for ([i (in-range 10)])
      (write (vector-ref m (xy->index i j))))
    (newline)))

(define (step! m)
  (define pending-flashes (incr-all! m))
  (define flashed (process-flashes! m pending-flashes (set)))
  (reset-cells! m flashed)
  (set-count flashed))

(define (solve! m)
  (for/sum ([i (in-range 1 101)])
    (step! m)))

(~> (open-aoc-input (find-session) 2021 11 #:cache #t)
    #;(open-input-string test-input)    
    load-map
    solve!)
