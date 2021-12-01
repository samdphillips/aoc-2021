#lang racket

(require advent-of-code
         threading)


(define input-stream
  (~> (open-aoc-input (find-session) 2021 1 #:cache #t)
      (in-port read _)
      sequence->stream))

(define sliding-stream
  (for/stream ([a input-stream]
               [b (stream-tail input-stream 1)]
               [c (stream-tail input-stream 2)])
    (+ a b c)))

(for/sum ([a sliding-stream]
          [b (stream-rest sliding-stream)]
          #:when (> b a))
  1)
