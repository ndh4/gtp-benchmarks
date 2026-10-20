#lang racket

(require "../../../ctcs/configurable.rkt" "../../../ctcs/precision-config.rkt")

(define/ctc-helper FIXME/c any/c)

(define string-amount/c
  (and/c
   string?
   (flat-named-contract 'amount (λ (x) (regexp-match #px"\\d*\\.\\d\\d" x)))))

(define/ctc-helper amount/c (and/c rational? exact?))

(provide (struct-out account)
         (struct-out dw)
         transactions->balance
         transactions->running-balance
         amount?
         amount->decimal-string
         align)

(provide amount> argv-amount> account/c)

(module+ examples (provide checking0))

(require "date.rkt")

(module+
 test
 (require (submod ".."))
 (require (submod ".." examples))
 (require rackunit))

(define/contract
 (amount? x)
 (configurable-ctc (max FIXME/c) (types (-> any/c boolean?)))
 (rational? x))

(define-match-expander
 amount>
 (λ (stx) (syntax-case stx () ((_ x) #'(app string->number/2 (? amount? x))))))

(define-match-expander
 argv-amount>
 (λ (stx)
   (syntax-case stx ()
     ((_ x) #'(app string->number/2 (and (? amount?) (? (>=/c 0) x)))))))

(define (string->number/2 amount)
  (when (regexp-match #px"\\.\\d\\d\\d" (~a amount))
    (eprintf "bad amount ~a\n" amount))
  (string->number amount))

(module+
 test
 (check-equal? (string->number/2 "1.23") 1.23)
 (check-match
  (with-output-to-string
   (λ ()
     (parameterize
      ((current-error-port (current-output-port)))
      (check-equal? (string->number/2 "1.234") 1.234))))
  (pregexp "bad")))

(define/contract
 (amount->decimal-string a)
 (configurable-ctc (max FIXME/c) (types (-> rational? string-amount/c)))
 (~r a #:precision '(= 2)))

(module+
 test
 (check-equal? (amount->decimal-string 3.0) "3.00")
 (check-equal? (amount->decimal-string 43.12) "43.12")
 (check-equal? (amount->decimal-string 43.1) "43.10")
 (check-equal? (amount->decimal-string 43.0) "43.00"))

(struct/contract
 dw
 ((comment (configurable-ctc (types string?) (max string?)))
  (date (configurable-ctc (types my-date/c) (max my-date/c)))
  (amount (configurable-ctc (types amount/c) (max amount/c))))
 #:transparent
 #:mutable)

(define/ctc-helper
 dw/c
 (configurable-ctc
  (types (struct/c dw string? my-date/c amount/c))
  (max (struct/c dw string? my-date/c amount/c))))

(struct/contract
 account
 ((name (configurable-ctc (types string?) (max string?)))
  (recents (configurable-ctc (types (listof dw/c)) (max (listof dw/c))))
  (balance (configurable-ctc (types amount/c) (max amount/c)))
  (balance-date (configurable-ctc (types my-date/c) (max my-date/c)))
  (history (configurable-ctc (types (listof dw/c)) (max (listof dw/c))))
  (check-no (configurable-ctc (types natural?) (max natural?))))
 #:transparent
 #:mutable)

(define/ctc-helper
 account/c
 (configurable-ctc
  (types
   (struct/c
    account
    string?
    (listof dw/c)
    amount/c
    my-date/c
    (listof dw/c)
    natural?))
  (max
   (struct/c
    account
    string?
    (listof dw/c)
    amount/c
    my-date/c
    (listof dw/c)
    natural?))))

(module+
 examples
 (define (checking0 (first-check-no 0))
   (account "checking" '() 0 (today) '() first-check-no)))

(define (new-account name #:first-check# (last 0))
  (account name '() 0 (today) '() last))

(define/contract
 (transactions->balance dw*)
 (configurable-ctc
  (types (-> (listof dw/c) amount/c))
  (max
   (->i
    ((dw* (listof dw/c)))
    (result
     (dw*)
     (and/c
      amount/c
      (lambda (res)
        (zero?
         (for/fold ((accum res)) ((h dw*)) (- accum (dw-amount h))))))))))
 (for/sum ((h dw*)) (dw-amount h)))

(define/contract
 (transactions->running-balance history current op)
 (configurable-ctc
  (types
   (->
    (listof dw/c)
    amount/c
    (-> amount/c amount/c amount/c)
    (listof (list/c dw/c amount/c))))
  (max
   (->i
    ((dws (listof dw/c))
     (balance amount/c)
     (op (-> amount/c amount/c amount/c)))
    (r
     (dws)
     (and (listof (list/c dw/c amount/c))
          (λ (r) (equal? dws (reverse (map first r)))))))))
 (for/fold
  ((balance current) (r '()) #:result r)
  ((h history))
  (define balance+ (op balance (dw-amount h)))
  (define dw+ (list h balance+))
  (values balance+ (cons dw+ r))))

(define COMMENT-MAX 30)

(define shorter-than-COMMENT-MAX
  (flat-named-contract
   "short purpose comment"
   (λ (s) (<= (string-length s) COMMENT-MAX))))

(define/contract
 (align purpose (x ""))
 (configurable-ctc
  (types (-> string? string?))
  (max
   (->
    (and/c string? shorter-than-COMMENT-MAX)
    (and/c string? (compose (=/c COMMENT-MAX) string-length)))))
 (~a
  " "
  x
  " "
  purpose
  #:max-width
  COMMENT-MAX
  #:min-width
  COMMENT-MAX
  #:left-pad-string
  " "))

(module+
 test
 (define LARGE (make-string COMMENT-MAX #\space))
 (check-equal? (string-length (align LARGE)) COMMENT-MAX "t")
 (check-equal? (string-length (align " Claudia")) COMMENT-MAX)
 (check-equal? (string-length (align " Claudia")) COMMENT-MAX)
 (check-equal? (string-length (align " C")) COMMENT-MAX))

(module+
 test
 (check-equal? (checking0) (new-account "checking"))
 (define dw0 (dw "test" (today) 10))
 (check-equal?
  (transactions->running-balance (list dw0) 10.0 -)
  (list (list dw0 0.0))
  "running balance: an account w/o initial deposit")
 (check-equal?
  (transactions->balance (list dw0))
  10
  "balance: an account w/o initial deposit"))

