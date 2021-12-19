#lang racket

;; Didn't work this one out until I read Ben Knoble's solution, then it
;; became obvious.
;; https://github.com/benknoble/advent2021/blob/main/day14/solution.rkt

(require advent-of-code
         syntax/parse/define
         threading)

(define test-input #<<```
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
```
  )

(define (read-template inp)
  (define template-s (read-line inp))
  (read-line inp)
  (values
   (for/fold ([pair-counts (hash)])
             ([i (in-range (sub1 (string-length template-s)))])
     (define k (substring template-s i (+ 2 i)))
     (hash-update pair-counts k add1 0))
   (for/fold ([counts (hash)]) ([c (in-string template-s)])
     (hash-update counts c add1 0))))

(define (make-rule-proc lch0 lch1 rch)
  (define d0 (string lch0 rch))
  (define d1 (string rch lch1))
  (define-syntax-rule (add n) (lambda (v) (+ n v)))
  (lambda (n pair-counts counts)    
    (values (~> (hash-update pair-counts d0 (add n) 0)
                (hash-update d1 (add n) 0))
            (hash-update counts rch (add n) 0))))

(define-match-expander ch
  (syntax-rules ()
    [(_ pat) (? string? (app (lambda (s) (string-ref s 0)) pat))]))

(define (read-rule inp)
  (match (read-line inp)
    [(? eof-object?) eof]
    [(regexp #px"((.)(.)) -> (.)" (list _ lhs (ch lch0) (ch lch1) (ch rch)))
     (cons lhs (make-rule-proc lch0 lch1 rch))]))

(define (read-puzzle inp)
  (define-values (pair-counts single-counts) (read-template inp))
  (values pair-counts
          single-counts
          (for/hash ([a-rule (in-port read-rule inp)])
            (values (car a-rule) (cdr a-rule)))))

(define (step rules pair-counts single-counts)
  (for/fold ([new-pair-counts (hash)]
             [single-counts single-counts])
            ([(pair count) (in-hash pair-counts)])
    (define rule (hash-ref rules pair))
    (rule count new-pair-counts single-counts)))

(define (calculate-final-result sc)
  (define-syntax-parse-rule (next f:id v0:id v1:id) (if v0 (f v0 v1) v1))
  (for/fold ([a #f] [b #f] #:result (- b a))
            ([v (in-hash-values sc)])
    (values (next min a v)
            (next max b v))))   

(define (run n rules pair-counts single-counts)
  (for/fold ([pc pair-counts]
             [sc single-counts]
             #:result (calculate-final-result sc))
            ([i n])
    (step rules pc sc)))

(define-values (pz-pair-counts pz-single-counts pz-rules)
  (~> #;(open-input-string test-input)
      (open-aoc-input (find-session) 2021 14 #:cache #t)
      read-puzzle))

(run 40 pz-rules pz-pair-counts pz-single-counts)