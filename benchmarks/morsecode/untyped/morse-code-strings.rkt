#lang racket/base

(require (only-in "morse-code-table.rkt" morse-string?)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         racket/contract
         racket/match
         racket/string)

(require (only-in
          "morse-code-table.rkt"
          char-table
          clean-pattern
          lines
          wikipedia-text))

(provide char->dit-dah-string string->morse)

(define/ctc-helper
 ((morse-decodes-to? str/char) morse-str)
 (define target-str (if (char? str/char) (string str/char) str/char))
 (define encoded-target
   (apply
    string-append
    (map
     (λ (c) (hash-ref char-table (char-downcase c)))
     (string->list target-str))))
 (string=? encoded-target morse-str))

(define/contract
 (char->dit-dah-string letter)
 (configurable-ctc
  (max
   (->i
    ((letter char?))
    #:pre
    (letter)
    (hash-has-key? char-table (char-downcase letter))
    (result (letter) (and/c morse-string? (morse-decodes-to? letter)))))
  (types (-> char? string?)))
 (define res (hash-ref char-table (char-downcase letter) #f))
 (if (eq? #f res)
   (raise-argument-error 'letter-map "character in map" 0 letter)
   res))

(define/contract
 (string->morse str)
 (configurable-ctc
  (max
   (->i
    ((str string?))
    (result (str) (and/c morse-string? (morse-decodes-to? str)))
    #:post
    (str result)
    (if (non-empty-string? str)
      (non-empty-string? result)
      (string=? result ""))))
  (types (-> string? string?)))
 (define morse-list (for/list ((c str)) (char->dit-dah-string c)))
 (apply string-append morse-list))

#;(module+
 test
 (require rackunit)
 (check-equal? (char->dit-dah-string #\a) ".-")
 (check-equal? (char->dit-dah-string #\b) "-...")
 (check-equal? (char->dit-dah-string #\s) "...")
 (check-equal? (char->dit-dah-string #\S) "...")
 (check-equal? (string->morse "sos") "...---...")
 (check-equal? (string->morse "wrought?") ".--.-.---..---.....-..--..")
 (check-equal? (string->morse "WROUGHT?") ".--.-.---..---.....-..--.."))

