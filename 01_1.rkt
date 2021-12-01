#lang racket

(require advent-of-code
         threading)

(define input-stream
  (~> (open-aoc-input (find-session) 2021 1 #:cache #t)
      (in-port read _)
      sequence->stream))

(for/sum ([a input-stream]
          [b (stream-rest input-stream)]
          #:when (> b a))
  1)
