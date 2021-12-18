#lang racket

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

(struct template (depth length layers) #:transparent)

(define (index-of j i)
  (cond
    [(zero? j) (values 0 i)]
    [(odd? i)  (values 0 (floor (/ i 2)))]
    [else
     (define-values (d k) (index-of (sub1 j) (/ i 2)))
     (values (add1 d) k)]))

(define (template-ref a-template i)
  (define-values (s t) (index-of (template-depth a-template) i))
  (vector-ref (list-ref (template-layers a-template) s) t))

(define (template->sequence a-template)
  (sequence-map (lambda~>> (template-ref a-template))
                (template-length a-template)))

#;
(define (template-flatten a-template)
  (template 0
            (template-length a-template)
            (list (for/vector ([v (template->sequence a-template)]) v))))

(define (read-template inp)
  (define template-s (read-line inp))
  (read-line inp)
  (define base
    (for/vector ([a-char (in-string template-s)])
      (string->symbol (string a-char))))
  (template 0 (vector-length base) (list base)))

(define-match-expander sym
  (syntax-rules ()
    [(_ pat) (app string->symbol pat)]))

(define (read-rule inp)
  (match (read-line inp)
    [(? eof-object?) eof]
    [(regexp #px"(.)(.) -> (.)" (list _ (sym a) (sym b) (sym c)))
     (list a b c)]))

(define (read-puzzle inp)
  (define init-template (read-template inp))
  (values init-template
          (for/fold ([rules (hasheq)])
                    ([a-rule (in-port read-rule inp)])
            (match-define (list in0 in1 out) a-rule)
            (hash-update rules in0
                         (lambda (o) (hash-set o in1 out))
                         hasheq))))

(define (rule-lookup rules u v)
  (~> rules
      (hash-ref u)
      (hash-ref v)))

(define (step-inserts rules a-template)
  (for/vector ([i (in-range (sub1 (template-length a-template)))])
    (rule-lookup rules
                 (template-ref a-template i)
                 (template-ref a-template (add1 i)))))

(define (step rules a-template)
  (define new-layer (step-inserts rules a-template))
  (template (add1 (template-depth a-template))
            (+ (template-length a-template) (vector-length new-layer))
            (cons new-layer
                  (template-layers a-template))))

(define (run n rules a-template)
  (for/fold ([t a-template]) ([i n])
    (step rules t)))    

(define (count-syms a-template)
  (for*/fold ([counts (hasheq)])
             ([layer (in-list (template-layers a-template))]
              [sym (in-vector layer)])
    (hash-update counts sym add1 0)))

(define (find-minmax counts)
  (define-syntax-parse-rule (next f:id v0:id v1:id) (if v0 (f v0 v1) v1))
  (for/fold ([u #f] [v #f]) ([c (in-hash-values counts)])
    (values (next min u c)
            (next max v c))))

(define-values (puzzle-template puzzle-rules)
  (~> #;(open-input-string test-input)
      (open-aoc-input (find-session) 2021 14 #:cache #t)
      read-puzzle))

;; part one
(~> (run 10 puzzle-rules puzzle-template)
    (count-syms)
    find-minmax)

;; part two - except it takes forever
#;
(~> (run 40 puzzle-rules puzzle-template)
    (count-syms)
    find-minmax)
