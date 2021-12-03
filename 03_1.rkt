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

(define (<< n s) (arithmetic-shift n s))

(define (common-bit ss i)
  (for/fold ([z 0] [o 0] #:result (if (> z o) 0 1)) ([l (in-list ss)])
    (match (string-ref l i)
      [#\0 (values (add1 z) o)]
      [#\1 (values z (add1 o))])))

(define (gamma-rate ss width)
  (for/fold ([gr 0]) ([i (in-range width)])
    (bitwise-ior (<< gr 1) (common-bit ss i))))

(define (epsilon-rate gr width)
  (define mask (sub1 (<< 1 width)))
  (bitwise-and mask (bitwise-not gr)))

(define (solve inp)
  (define lines (port->lines inp))
  (define width (string-length (car lines)))
  (define gr (gamma-rate lines width))
  (define er (epsilon-rate gr width))
  (values gr er))

#;
(solve (open-input-string test-input))


(solve (open-aoc-input (find-session) 2021 3 #:cache #t))