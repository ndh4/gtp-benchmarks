#lang racket

(require "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require (only-in "date.rkt" my-date/c))

(define/ctc-helper
 (generate-amount-string fuel)
 (define max-int (expt 10 fuel))
 (thunk
  (define int-part
    (if (zero? (random 2)) "" (number->string (random max-int))))
  (define dec-part (~r (random 100) #:min-width 2 #:pad-string "0"))
  (string-append int-part "." dec-part)))

(define/ctc-helper
 string-amount/c
 (flat-named-contract
  'string-amount/c
  (and/c string? #px"\\d*\\.\\d\\d")
  generate-amount-string))

(define/ctc-helper amount/c (and/c rational? exact?))

(define/ctc-helper
 (valid-bmp-integer? n)
 (and (>= n 0) (<= n 65535) (not (<= 55296 n 57343))))

(define/ctc-helper
 (ascii-file-safe-char? c)
 (define n (char->integer c))
 (and (>= n 32) (<= n 126) (not (char=? c #\/)) (not (char=? c #\\))))

(define bmp-char?
  (flat-named-contract
   'bmp-char?
   ascii-file-safe-char?
   (lambda (fuel)
     (lambda ()
       (let loop ()
         (define n (+ 32 (random 95)))
         (define c (integer->char n))
         (if (ascii-file-safe-char? c) c (loop)))))))

(define bmp-string?
  (flat-named-contract
   'bmp-string?
   (lambda (s) (and (string? s) (for/and ((c (in-string s))) (bmp-char? c))))
   (lambda (fuel)
     (define charlist-gen
       (contract-random-generate/choose (listof bmp-char?) fuel))
     (lambda ()
       (define s (list->string (charlist-gen)))
       (if (member s '("" "." "..")) "safe_default_name.txt" s)))))

(define/ctc-helper
 pathable-string/c
 (and/c bmp-string? (string-len/c 60) path-string?))

(provide (struct-out account)
         (struct-out dw)
         string->number/2
         pathable-string/c)

(provide transactions->balance
         transactions->running-balance
         amount?
         amount->decimal-string
         align)

(provide amount> argv-amount> amount/c string-amount/c account/c dw/c)

(module+ examples (provide checking0))

(require (only-in "date.rkt" today))

(module+
 test
 (require (submod ".."))
 (require (submod ".." examples))
 (require rackunit))

(define/contract
 (amount? x)
 (configurable-ctc
  (max
   (->i ((obj any/c)) (res (obj) ((flat-contract-predicate amount/c) obj))))
  (types (-> any/c boolean?)))
 (and (rational? x) (exact? x)))

(module+ test (check-true (amount? 0)))

(define-match-expander
 amount>
 (λ (stx) (syntax-case stx () ((_ x) #'(app string->number/2 (? amount? x)))))
 (λ (stx)
   (syntax-case stx ()
     ((_ expr)
      #'(cond
         ((number? expr) (inexact->exact expr))
         ((string->number/2 expr)
          (λ (x)
            (if (amount? x) x (error 'amount> "amount expected, given ~s" x))))
         (else (error 'amount> "amount expected, given ~s" expr)))))))

(define-match-expander
 argv-amount>
 (λ (stx)
   (syntax-case stx ()
     ((_ x)
      #'(app
         string->number/2
         (and (? amount?) (? (lambda (x) (>= x 0)) x)))))))

(module+
 test
 (match "159.20" ((argv-amount> x) (check-false (inexact? x) "argv match")))
 (match "0." ((argv-amount> x) (check-true (= x 0)))))

(define (string->number/2 amount)
  (when (regexp-match #px"\\.\\d\\d\\d" (~a amount))
    (eprintf "bad amount ~a\n" amount))
  (parameterize ((read-decimal-as-inexact #f)) (string->number amount)))

(module+
 test
 (check-equal? (string->number/2 "1.23") 123/100)
 (check-match
  (with-output-to-string
   (λ ()
     (parameterize
      ((current-error-port (current-output-port)))
      (check-equal? (string->number/2 "1.234") 1.234))))
  (pregexp "bad")))

(define/contract
 (amount->decimal-string a)
 (configurable-ctc
  (max (-> amount/c string-amount/c))
  (types (-> amount/c string-amount/c)))
 (~r a #:precision '(= 2)))

(module+
 test
 (check-equal? (amount->decimal-string 3.0) "3.00")
 (check-equal? (amount->decimal-string 43.12) "43.12")
 (check-equal? (amount->decimal-string 43.1) "43.10")
 (check-equal? (amount->decimal-string 43.0) "43.00"))

(struct dw (comment date amount) #:transparent #:mutable)

(define/ctc-helper
 dw/c
 (configurable-ctc
  (types (struct/c dw string? my-date/c amount/c))
  (max (struct/c dw (min-len-string/c 1) my-date/c amount/c))))

(struct
 account
 (name recents balance balance-date history check-no special)
 #:transparent
 #:mutable)

(define/ctc-helper
 account/c
 (configurable-ctc
  (types
   (struct/c
    account
    pathable-string/c
    (listof dw/c)
    amount/c
    my-date/c
    (listof dw/c)
    natural-number/c
    (listof (list/c string? amount/c))))
  (max
   (struct/c
    account
    pathable-string/c
    (listof dw/c)
    amount/c
    my-date/c
    (listof dw/c)
    natural-number/c
    (listof (list/c string? amount/c))))))

(module+
 examples
 (define (checking0 (first-check-no 0))
   (account "checking" '() 0 (today) '() first-check-no '())))

(define (new-account name #:first-check# (last 0))
  (account name '() 0 (today) '() last '()))

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
  (transactions->running-balance (list dw0) 10 -)
  (list (list dw0 0))
  "running balance: an account w/o initial deposit")
 (check-equal?
  (transactions->balance (list dw0))
  10
  "balance: an account w/o initial deposit"))

