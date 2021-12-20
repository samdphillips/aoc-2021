#lang racket

(require advent-of-code
         bitsyntax
         threading)

(define test-literal (bytes #xD2 #xFE #x28))
(define test-operator1 (bytes #x38 #x00 #x6F #x45 #x29 #x12 #x00))
(define test-operator2 (bytes #xEE #x00 #xD4 #x0C #x82 #x30 #x60))

(define (char->nybble c)
  (define i (char->integer c))
  (cond
    [(< i 65) (- i 48)]
    [else (+ 10 (- i 65))]))

(define (string->bits s)
  (call-with-output-bytes
   (lambda (outp)
     (for ([i (in-range 0 (sub1 (string-length s)) 2)])
       (write-byte (+ (* 16 (char->nybble (string-ref s i)))
                      (char->nybble (string-ref s (add1 i))))
                   outp)))))

(struct $packet (version) #:transparent)
(struct $literal $packet (value) #:transparent)
(struct $operator $packet (type packets) #:transparent)

(define ($packet-subpackets pkt)
  (cond
    [($operator? pkt) ($operator-packets pkt)]
    [else null]))

(define ($packet-version-sum pkt)
  (+ ($packet-version pkt)
     (for/sum ([subpkt (in-list ($packet-subpackets pkt))])
       ($packet-version-sum subpkt))))

(define (decode-literal-value input-bits sk fk)
  (define (accum acc n) (bitwise-ior (arithmetic-shift acc 4) n))
  (define (decode4 input-bits acc)
    (bit-string-case input-bits
      [({= 0 :: bits 1} {n :: bits 4} {rest :: binary})
       (sk (accum acc n) rest)]
      [({= 1 :: bits 1} {n :: bits 4} {rest :: binary})
       (decode4 rest (accum acc n))]))
  (decode4 input-bits 0))

(define-syntax literal-value
  (syntax-rules ()
    [(_ #t input sk fk) (decode-literal-value input sk fk)]))

(define (decode-packet* input)
  (define-values (pkt rest) (decode-packet input))
  (cond
    [(zero? (bit-string-length rest)) (list pkt)]
    [else (cons pkt (decode-packet* rest))]))

(define (decode-packet*/count n input)
  (cond
    [(zero? n) (values null input)]
    [else
     (define-values (pkt next) (decode-packet input))
     (define-values (pkt* rest) (decode-packet*/count (sub1 n) next))
     (values (cons pkt pkt*) rest)]))     
  
(define (decode-packet input)
  (bit-string-case input
    [({version :: bits 3}
      {= #b100 :: bits 3}
      {value   :: (literal-value)}
      {rest    :: binary})
     (values ($literal version value) rest)]
    [({version     :: bits 3}
      {type        :: bits 3}
      {= 0         :: bits 1}
      {length      :: bits 15}
      {sub-packets :: binary bits length}
      {rest        :: binary})
     (values ($operator version type (decode-packet* sub-packets)) rest)]
    [({version     :: bits 3}
      {type        :: bits 3}
      {= 1         :: bits 1}
      {length      :: bits 11}
      {payload     :: binary})
     (define-values (sub-packets rest) (decode-packet*/count length payload))
     (values ($operator version type sub-packets) rest)]))
   
(define (decode-packet-top input)
  (define-values (pkt discard) (decode-packet input))
  pkt)

(define (solve str)
  (~> str
      string->bits
      decode-packet-top
      $packet-version-sum))

(~> (open-aoc-input (find-session) 2021 16 #:cache #t)
    port->string
    solve)