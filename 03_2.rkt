#lang racket

(require advent-of-code)

(define test-input #<<DATA
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
DATA
  )

(define (single-item? vs) (null? (cdr vs)))

(define (common-bit ss i compute-result)
  (for/fold ([z 0] [o 0] #:result (compute-result z o)) ([l (in-list ss)])
    (match (string-ref l i)
      [#\0 (values (add1 z) o)]
      [#\1 (values z (add1 o))])))

(define (filter-bit ss i v)
  (for/list ([l (in-list ss)] #:when (char=? (string-ref l i) v)) l))

(define (calculate-rating ss compute-result)
  (define (run ss i)
    (cond
      [(single-item? ss) (string->number (car ss) 2)]
      [else
       (define c (common-bit ss i compute-result))
       (run (filter-bit ss i c) (add1 i))]))
  (run ss 0))

(define (make-compute-results a b)
  (lambda (z o)
    (cond
      [(= z o) a]
      [(> z o) b]
      [else    a])))

(define (o2-rating ss) 
  (calculate-rating ss (make-compute-results #\1 #\0)))

(define (co2-rating ss)
  (calculate-rating ss (make-compute-results #\0 #\1)))

(define (solve inp)
  (define lines (port->lines inp))
  (values (o2-rating lines)
          (co2-rating lines)))

#;
(solve (open-input-string test-input))

(solve (open-aoc-input (find-session) 2021 3 #:cache #t))