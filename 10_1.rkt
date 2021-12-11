#lang racket

(require advent-of-code
         threading)

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
                                         'incomplete))
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
  (hash #\) 3 #\] 57 #\} 1197 #\> 25137))

(define (score v)
  (match v
    [(list 'corrupted _ _ actual) (hash-ref scores actual)]
    [_ 0]))

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 10 #:cache #t)
    port->lines
    (map (lambda~> categorize-line score) _)
    (apply + _))
