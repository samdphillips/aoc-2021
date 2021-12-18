#lang racket

(require advent-of-code
         graph
         racket/generator
         racket/set
         threading)

(define test-input #<<```
start-A
start-b
A-c
A-b
b-d
A-end
b-end
```
  )

(define (read-edge inp)
  (match (read-line inp)
    [(? eof-object?) eof]
    [(regexp #px"^([^-]+)-([^-]+)$" (list _ src dest)) (list src dest)]))

(define (large-cave? s)
  (not (small-cave? s)))

(define (small-cave? s)
  (char-lower-case? (string-ref s 0)))

(struct visits (trail seen duped?) #:transparent)

(define (visits-add vs v)
  (visits (cons v (visits-trail vs))
          (set-add (visits-seen vs) v)
          (or (visits-duped? vs)
              (and (small-cave? v)
                   (set-member? (visits-seen vs) v)
                   v))))

(define (can-visit? vs v)
  (cond
    [(large-cave? v) #t]
    [(string=? v "start") #f]
    [(set-member? (visits-seen vs) v) (not (visits-duped? vs))]
    [else #t]))

;; find all paths from "start" to "end" in g
(define (find-paths g)
  (in-generator
   (define (explore src vs)
     (cond
       [(string=? src "end") (yield vs)]
       [else
        (for ([n (in-neighbors g src)]
              #:when (can-visit? vs n))
          (explore n (visits-add vs n)))]))
   (explore "start" (visits null (set) #f))))

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 12 #:cache #t)
    (port->list read-edge _)
    unweighted-graph/undirected
    find-paths
    sequence-length)