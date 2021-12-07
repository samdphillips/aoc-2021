#lang racket

(require advent-of-code         
         threading)

(provide (all-defined-out))

(define test-input "3,4,3,1,2")

(define (vector-update! vec i f)
  (vector-set! vec i (f (vector-ref vec i))))

(define (load-age-vector vs)
  (define vec (make-vector 9 0))
  (for ([i (in-list vs)]) (vector-update! vec i add1))
  vec)

(define load-input
  (lambda~> read-line
            (string-split ",")
            (map string->number _)
            load-age-vector))

(define (step vec)
  (define (ref i) (vector-ref vec i))
  (vector (ref 1)
          (ref 2)
          (ref 3)
          (ref 4)
          (ref 5)
          (ref 6)
          (+ (ref 0) (ref 7))
          (ref 8)
          (ref 0)))

(define (total vec) (for/sum ([v vec]) v))

(define (solve vec [t 80])
  (for/fold ([v vec] #:result (total v)) ([i (in-range t)])
    (step v)))

(module* main #f
  (~> #;(open-input-string test-input)
      (open-aoc-input (find-session) 2021 6 #:cache #t)
      load-input
      solve))
