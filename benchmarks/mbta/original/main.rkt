#lang racket

(require "data.rkt"
         "helpers.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         (only-in racket/string string-join))

(require (only-in "run-t.rkt" run-t manage EOM DONE ENABLE DISABLE PATH))

(provide dat->station-names BLUE-STATIONS ORANGE-STATIONS path enable disable)

(define/contract
 (dat->station-names fname)
 (configurable-ctc
  (max
   (->i
    ((fname (and/c path-string? (λ (f) (file-exists? f)))))
    (result
     (fname)
     (and/c (listof station?) (λ (lst) (sublist? lst (file->lines fname)))))))
  (types (-> string? (listof string?))))
 (for/list
  ((line (in-list (file->lines fname)))
   #:when
   (and (< 0 (string-length line)) (not (eq? #\- (string-ref line 0)))))
  (string-trim line)))

(define/contract
 BLUE-STATIONS
 (configurable-ctc
  (max
   (and/c
    (listof station?)
    (λ (lst) (sublist? lst (file->lines "../base/blue.dat")))))
  (types (listof string?)))
 (dat->station-names "../base/blue.dat"))

(define/contract
 ORANGE-STATIONS
 (configurable-ctc
  (max
   (and/c
    (listof station?)
    (λ (lst) (sublist? lst (file->lines "../base/orange.dat")))))
  (types (listof string?)))
 (dat->station-names "../base/orange.dat"))

(define/contract
 (path from to)
 (configurable-ctc
  (max
   (->i
    ((from string?) (to string?))
    (result
     (from to)
     (λ (res) (ordered-substrings? (list "from" from "to" to) res)))))
  (types (-> string? string? string?)))
 (format "from ~a to ~a" from to))

(define/contract
 (enable s)
 (configurable-ctc
  (max
   (->i
    ((s string?))
    (result (s) (λ (res) (ordered-substrings? (list "enable" s) res)))))
  (types (-> string? string?)))
 (format "enable ~a" s))

(define/contract
 (disable s)
 (configurable-ctc
  (max
   (->i
    ((s string?))
    (result (s) (λ (res) (ordered-substrings? (list "disable" s) res)))))
  (types (-> string? string?)))
 (format "disable ~a" s))

(module+
 test
 (require rackunit)
 (define (run-query str)
   (define r (run-t str))
   (if r r (error 'main (format "run-t failed to respond to query ~e\n" str))))
 (define (num-pieces res) (length (string-split res "\n")))
 (check-equal? (num-pieces (run-query (path "Airport" "Northeastern"))) 14)
 (define res1 (run-query (disable "Government")))
 (check-equal? (num-pieces res1) 1)
 (check-equal? (num-pieces (run-query (path "Airport" "Northeastern"))) 16)
 (define res2 (run-query (enable "Government")))
 (check-equal? (num-pieces res2) 1)
 (check-equal? (num-pieces (run-query (path "Airport" "Harvard Square"))) 12)
 (define res3 (run-query (disable "Park Street")))
 (check-equal? (num-pieces res3) 1)
 (check-equal?
  (num-pieces (run-query (path "Northeastern" "Harvard Square")))
  1)
 (define res4 (run-query (enable "Park Street")))
 (check-equal? (num-pieces res4) 1)
 (check-equal?
  (num-pieces (run-query (path "Northeastern" "Harvard Square")))
  12))

