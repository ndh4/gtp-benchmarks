#lang racket/base

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; -----------------------------------------------------------------------------

(require
  require-typed-check
  (only-in racket/file file->value)
  "../../../ctcs/configurable.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  racket/contract
  racket/random)

;; (require (only-in "morse-code-strings.rkt"
  ;; string->morse))
(require/configurable-contract "morse-code-strings.rkt" string->morse )

;; (require (only-in "levenshtein.rkt"
               ;; string-levenshtein))
(require/configurable-contract "levenshtein.rkt" string-levenshtein )

;; (provide/configurable-contract
;;  [word-frequency-list string?]
;;  [word-frequency-list-small string?]
;;  [file->words (-> path-string? (listof string?))]
;;  [allwords (listof string?)]
;;  [words-small (listof string?)]
;;  [words-smaller (listof string?)]
;;  [main (-> (listof string?) void?)])


(define word-frequency-list
  "./../base/frequency.rktd")
(define word-frequency-list-small
  "./../base/frequency-small.rktd")

(define (file->words filename)
  (define words+freqs (file->value (string->path filename)))
  (for/list ([word+freq  words+freqs])
    (car word+freq)))

(define allwords (file->words word-frequency-list))

(define words-small
  (file->words word-frequency-list-small))

;; ll: `words-small` far too large
(define words-smaller
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed 42)
    (random-sample words-small 10
                   #:replacement? #f)))

(define (main words)
  (for* ([w1 (in-list words)]
         [w2 (in-list words)])
    (string->morse w1)
    (string->morse w2)
    (string-levenshtein w1 w2)
    ;; ll: this gets done by the loop anyway: no need to double it
    ;; (string-levenshtein w2 w1)
    (void)))

(time (main words-smaller))
