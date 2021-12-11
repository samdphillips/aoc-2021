#lang racket

(require advent-of-code
         threading
         (only-in unstable/match
                  match?))

(define test-input #<<DATA
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
DATA
  )

(define (categorize-line s)
  (let/ec ret
    (define (push stk v) (cons v stk))
    (define (pop-check stk actual)
      (match stk
        [(cons (== actual) stk) stk]
        [(cons expected _) (ret (list 'corrupted s expected actual))]))
    (for/fold ([stack null] #:result (if (null? stack)
                                         'complete
                                         (list 'incomplete s stack)))
              ([ch (in-string s)])
      (match ch
        [#\( (push stack #\))]
        [#\{ (push stack #\})]
        [#\[ (push stack #\])]
        [#\< (push stack #\>)]
        [#\) (pop-check stack #\))]
        [#\} (pop-check stack #\})]
        [#\] (pop-check stack #\])]
        [#\> (pop-check stack #\>)]))))

(define scores
  (hash #\) 1 #\] 2 #\} 3 #\> 4))

(define (score v)
  (match-define (list 'incomplete _ stk) v)
  (for/fold ([s 0]) ([ch (in-list stk)])
    (+ (* s 5) (hash-ref scores ch))))

(define (incomplete? v)
  (match? v (list 'incomplete _ _)))

(define (middle-value vs)
  (define vec (list->vector vs))
  (define s (vector-length vec))
  (vector-ref vec (floor (/ s 2))))

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 10 #:cache #t)
    port->lines
    (map categorize-line _)
    (filter incomplete? _)
    (map score _)
    (sort <)
    middle-value)
