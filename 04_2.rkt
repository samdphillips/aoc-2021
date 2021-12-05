#lang racket

(require advent-of-code
         racket/generator
         threading)

(define test-input #<<DATA
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
DATA
  )

(struct puzzle (draws boards) #:transparent)

(define (puzzle-step p)
  (match-define
    (puzzle (list d next-draws ...) boards) p)
  (define new-boards
    (for/list ([b (in-list boards)]) (board-mark b d)))
  (puzzle next-draws new-boards))

(define (puzzle-winner p)
  (for/first ([b (in-list (puzzle-boards p))] #:when (board-winner? b)) b))

(define (puzzle-solutions p)
  (generator ()
    (define (remove-board p b)
      (struct-copy
       puzzle p
       [boards (for/list ([bb (in-list (puzzle-boards p))]
                          #:unless (eq? b bb))
                 bb)]))
    (define (puzzle-solve p)      
      (cond
        [(null? (puzzle-draws p)) (yield #f)]
        [(puzzle-winner p)
         =>
         (lambda (b)
           (yield b)
           (puzzle-solve (remove-board p b)))]            
        [else
         (puzzle-solve (puzzle-step p))]))
    (puzzle-solve p)))

(struct board (cells marks) #:transparent)

(define (vector-index-of vec val)
  (for/first ([v (in-vector vec)]
              [i (in-naturals)]
              #:when (= val v)) i))

(define (index->board-pos i) (quotient/remainder i 5))

(define (board-mark b v)
  (define idx (vector-index-of (board-cells b) v))
  (cond
    [idx (struct-copy board b
                      [marks (cons idx (board-marks b))])]
    [else b]))

(define (board-winner? b)
  (board-marks-winner? (board-marks b)))

(define (board-marks-winner? marks)
  (define counts (make-vector 10 0))
  (define (update-counts! i)
    (define v (vector-ref counts i))
    (cond
      [(= v 4) #t]
      [else
       (vector-set! counts i (add1 v))
       #f]))
  (for/or ([m (in-list marks)])
    (define-values (row col) (index->board-pos m))
    (or (update-counts! row)
        (update-counts! (+ 5 col)))))

(define (board-score b)
  (define cells (board-cells b))
  (define marks (board-marks b))
  (define last-draw (vector-ref cells (car marks)))
  (define board-sum
    (for/sum ([v (in-vector cells)]
              [i (in-naturals)]
              #:unless (member i marks)) v))
  (* last-draw board-sum))

(define (string-whitespace? s)
  (regexp-match? #px"^\\w*$" s))

(define (read-puzzle-input inp)
  (define draws (read-draws inp))
  (define boards
    (for/list ([board (in-port read-board inp)]) board))
  (puzzle draws boards))

(define (read-draws inp)
  (~> (read-line inp)
      (string-split #px",")
      (map string->number _)))

(define (read-board inp)
  (let/ec ret
    (when (eof-object? (peek-char inp))
      (ret eof))
    (let ([b (read-line inp)])
      (unless (string-whitespace? b)
        (error 'read-board "expected blank line between boards, got: ~a" b)))
    (define cells
      (vector->immutable-vector
       (for/vector #:length 25 ([v (in-port read inp)]) v)))
    (board cells null)))

(define (dbg v)
  (displayln "-----")
  (pretty-print v)
  (displayln (and (puzzle-winner v) #t))
  v)

(~> #;(open-input-string test-input)
    (open-aoc-input (find-session) 2021 4 #:cache #t)
    read-puzzle-input
    puzzle-solutions
    (in-producer #f)
    ((lambda (seq)
       (for/last ([v seq]) v)))
    board-score)


