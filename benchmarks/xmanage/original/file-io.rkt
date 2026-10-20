#lang racket

(require "../../../ctcs/configurable.rkt" "../../../ctcs/precision-config.rkt")

(define balanced-account/c
  (and account?
       (flat-named-contract
        "current balance equals sum of transactions"
        (λ (a)
          (=
           (transactions->balance (account-history a))
           (account-balance a))))))

(provide account-reader account-writer)

(module+ examples (provide (all-defined-out)))

(require "date.rkt")

(require "data.rkt")

(module+
 examples
 (require (submod "data.rkt" examples))
 (provide (all-defined-out))
 (current-seconds/maybe-fake (thunk 1784630918)))

(module+
 test
 (require (submod ".." examples))
 (require (submod ".."))
 (require rackunit))

(module+
 examples
 (define 100purpose "one deposit")
 (define 50purpose "one withdrawal")
 (define 25purpose "one check written")
 (define (one-recent-plus-100 A (d "100.00") (msg 100purpose))
   (match
    A
    ((list name recents balance date history)
     (define dw (list (align msg) '(2026 6 6) d))
     (list name (cons dw recents) balance date history))))
 (define (one-recent-minus-100 A)
   (match
    A
    ((list name recents balance date history)
     (define dw (list (align 50purpose) '(2026 6 6) "-50.00"))
     (list name (cons dw recents) balance date history))))
 (define (Achecking0 (x 0)) (account->A (checking0 x)))
 (define checking (checking0))
 (define A (account->A checking))
 (define Achk (A->account A 0))
 (define A+100 (one-recent-plus-100 (Achecking0)))
 (define A+100chk (A->account A+100 0))
 (define 1-25purpose
   (~a
    (add1 (account-check-no A+100chk))
    " "
    25purpose
    #:max-width
    30
    #:min-width
    30
    #:left-pad-string
    " "))
 (define A+100+25
   (one-recent-plus-100
    (one-recent-plus-100 (Achecking0 1))
    "-25.00"
    1-25purpose))
 (define A+100+25chk (A->account A+100+25 1))
 (define A+100-50 (one-recent-minus-100 (one-recent-plus-100 (Achecking0))))
 (define A+100-50chk (A->account A+100-50 0))
 (define chase-chk
   (with-input-from-file "../.sample.act" (λ () (account-reader)))))

(define/contract
 (account-reader)
 (configurable-ctc (types (-> account?)) (max (-> balanced-account/c)))
 (A->account (read) (read)))

(define (A->account x last-check#)
  (parameterize
   ((read-decimal-as-inexact #f))
   (match
    x
    ((list
      (? string? name)
      (app list*->dw recents)
      (amount> balance)
      (? my-date? date)
      (app list*->dw history))
     (define expected-balance (transactions->balance history))
     (unless (= expected-balance balance)
       (error
        'account-reader
        "~a history does not add up to balance, found ~a vs ~a (delta = ~a)"
        name
        (exact->inexact balance)
        (exact->inexact expected-balance)
        (exact->inexact (- balance expected-balance))))
     (account name recents balance date history last-check#))
    (_
     (error 'A->account "A expression expected, found ~a ~a" x last-check#)))))

(define (list*->dw x)
  (unless (list? x) (error 'list*->dw "[listof DWList] expected, found ~a" x))
  (map list->dw x))

(define (list->dw xy)
  (match
   xy
   ((list (? string? comment) (? my-date? date) (? amount? amount))
    (dw comment date amount))
   ((list (? string? comment) (? my-date? date) (amount> amount))
    (dw comment date amount))
   (_ (error 'list->dw "DWList expected, found ~a" xy))))

(module+
 test
 (check-equal?
  (list->dw (list "a" '(2026 6 6) -100))
  (dw "a" '(2026 6 6) -100)
  "coverage 1"))

(define/contract
 (account-writer x)
 (configurable-ctc
  (types (-> account? void?))
  (max (-> balanced-account/c void?)))
 (pretty-write (account->A x))
 (display (account-check-no x))
 (display "; last check written"))

(define (account->A x)
  (list
   (account-name x)
   (map dw->list (account-recents x))
   (amount->decimal-string (account-balance x))
   (account-balance-date x)
   (map dw->list (account-history x))))

(define (dw->list dw)
  (list (dw-comment dw) (dw-date dw) (amount->decimal-string (dw-amount dw))))

(module+
 test
 (check-equal? (A->account A (account-check-no checking)) checking "new")
 (check-equal? (account->A (A->account A+100 0)) A+100 "plus")
 (check-equal? (account->A (A->account A+100-50 0)) A+100-50 "plus"))

(module+
 test
 (define A+100-50-file
   (with-output-to-string
    (λ () (define a (A->account A+100-50 0)) (account-writer a))))
 (check-equal?
  (with-input-from-string A+100-50-file account-reader)
  (A->account A+100-50 0)
  "basic I/O back and forth"))

(module+
 test
 (check-true
  (account? (with-input-from-file "../.sample.act" account-reader))
  "check consistency of chase account for validity"))

