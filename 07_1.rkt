#lang racket

(require advent-of-code
         threading)

(define test-input "16,1,2,0,4,2,7,1,2,14")

(define (list->counts vs)
  (define (hash->counts h)
    (for/list ([(k v) (in-hash h)]) (list k v)))
  (for/fold ([h (hash)] #:result (hash->counts h))
            ([v (in-list vs)])
    (hash-update h v add1 0)))

(define load-input
  (lambda~> read-line
            (string-split ",")
            (map string->number _)
            list->counts))

(define (cost1 src amt dest)
  (* amt (abs (- dest src))))

(define (cost counts dest)
  (for/sum ([c (in-list counts)])
    (cost1 (first c) (second c) dest)))

(define (find-min-cost counts)
  (define-syntax-rule (mm f)
    (for/fold ([v #f]) ([w (in-list counts)])
      (cond
        [(not v) (first w)]
        [else (f v (first w))])))
  (define a (mm min))
  (define b (add1 (mm max)))
  (for/fold ([pos #f] [fuel #f]) ([x (in-range a b)])
    (define f (cost counts x))
    (cond
      [(or (not pos) (< f fuel)) (values x f)]
      [else (values pos fuel)])))       

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 7 #:cache #t)
    load-input
    find-min-cost)
