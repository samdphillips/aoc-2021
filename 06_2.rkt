#lang racket

(require advent-of-code
         threading
         "06_1.rkt")

(module* main #f
  (~> #;(open-input-string test-input)
      (open-aoc-input (find-session) 2021 6 #:cache #t)
      load-input
      (solve 256)))
