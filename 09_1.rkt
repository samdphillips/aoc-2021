#lang racket

(require advent-of-code
         threading)

(define test-input #<<DATA
2199943210
3987894921
9856789892
8767896789
9899965678
DATA
  )


(define (xy->index c x y) (+ x (* y c)))
(define (index->xy c i)
  (define-values (y x) (quotient/remainder i c))
  (values x y))

(define (load-map inp)
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
  (values cols rows cells))

(define (min-neighbors cols rows vec i)
  (define-values (x y) (index->xy cols i))
  (define val (vector-ref vec i))
  (define-syntax-rule (check-dir xe ye)
    (let ([x xe] [y ye])
      (cond
        [(or (< x 0) (<= cols x)
             (< y 0) (<= rows y)) #t]
        [else
         (define i (xy->index cols x y))
         (< val (vector-ref vec i))])))
  (and (check-dir x (sub1 y))  ;; N
       (check-dir x (add1 y))  ;; S
       (check-dir (sub1 x) y)  ;; W
       (check-dir (add1 x) y)  ;; E
       val))

(define-values (cols rows store)
  (~> #;(open-input-string test-input)
      (open-aoc-input (find-session) 2021 9 #:cache #t)
      load-map))

(for/sum ([i (in-range (vector-length store))])
  (cond [(min-neighbors cols rows store i) => add1]
        [else 0]))