#lang racket

(require advent-of-code
         threading)

(define test-input #<<DATA
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
DATA
  )

(define (read-puzzle-input inp)
  (for/list ([line (in-lines inp)])
    (match-define (list inputs outputs)
      (string-split line " | "))
    (list (string-split inputs)
          (string-split outputs))))

(define puzzle-ins first)
(define puzzle-outs second)

(define (count-unique-outputs test-sets)
  (for/sum ([io (in-list test-sets)])
    (for/sum ([out (in-list (puzzle-outs io))])
      (match (string-length out)
        [(or 2 3 4 7) 1]
        [_ 0]))))

(~> (open-aoc-input (find-session) 2021 8 #:cache #t)
    #;(open-input-string test-input)
    read-puzzle-input
    count-unique-outputs)
