#lang racket/base

(require require-typed-check
         (only-in racket/file file->value)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         racket/contract
         racket/random)

(require (only-in "morse-code-strings.rkt" string->morse))

(require (only-in "levenshtein.rkt" string-levenshtein))

(define word-frequency-list "./../base/frequency.rktd")

(define word-frequency-list-small "./../base/frequency-small.rktd")

(define (file->words filename)
  (define words+freqs (file->value (string->path filename)))
  (for/list ((word+freq words+freqs)) (car word+freq)))

(define allwords (file->words word-frequency-list))

(define words-small (file->words word-frequency-list-small))

(define words-smaller
  (parameterize
   ((current-pseudo-random-generator (make-pseudo-random-generator)))
   (random-seed 42)
   (random-sample words-small 10 #:replacement? #f)))

(define (main words)
  (for*
   ((w1 (in-list words)) (w2 (in-list words)))
   (string->morse w1)
   (string->morse w2)
   (string-levenshtein w1 w2)
   (void)))

