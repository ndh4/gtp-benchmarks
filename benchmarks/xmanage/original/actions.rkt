#lang racket

(require "../../../ctcs/configurable.rkt" "../../../ctcs/precision-config.rkt")

(define (action/c x/c) (list/c x/c (-> any)))

(define/ctc-helper FIXME/c any/c)

(provide (rename-out (to-html to-html/t)))

(provide name->path
         write
         update
         new-account/2
         deposit
         withdraw
         write-check
         show-balance
         to-html)

(require "date.rkt")

(require "data.rkt")

(require "file-io.rkt")

(require "html.rkt")

(require racket/control)

(require (only-in xml xexpr->xml display-xml/content))

(module+
 test
 (require (submod ".."))
 (require (submod "file-io.rkt" examples))
 (require rackunit)
 (current-seconds/maybe-fake (thunk 1784630918)))

(define/contract
 (name->path name #:kind (proper-or-html #f))
 (configurable-ctc
  (max FIXME/c)
  (types (->* (string?) (#:kind string?) path?)))
 (define file-name
   (if proper-or-html (~a name "." proper-or-html) (~a "." name ".act")))
 (build-path file-name))

(define ((write account))
  (define file-name (name->path (account-name account)))
  (with-output-to-file
   file-name
   #:exists
   'replace
   (λ () (account-writer account))))

(module+
 test
 (check-equal? (name->path "name") (build-path ".name.act"))
 (check-equal? (name->path "n" #:kind "h") (build-path "n.h"))
 (let ((file-name (name->path (account-name checking))))
   (dynamic-wind
    void
    (λ () (check-true (void? ((write checking))) "dumb check"))
    (λ () (delete-file file-name)))))

(define ALREADY-EXISTS "An account with this name already exists.\n")

(define/contract
 (new-account/2 exit-for-testing name #:last-check (last 0))
 (configurable-ctc
  (max FIXME/c)
  (types (-> procedure? string? (action/c account?))))
 (define a (account name '() 0 (today) '() last))
 (define (effect)
   (define file-name (name->path name))
   (when (file-exists? file-name) (printf ALREADY-EXISTS) (exit-for-testing 1))
   (with-output-to-file file-name (λ () (account-writer a))))
 (list a effect))

(define ((make-d/w how) account date a . msg)
  (define amount
    (match
     a
     ((argv-amount> b) b)
     (_ (error 'xmanage "amount expected, found ~a" a))))
  (define history (account-recents account))
  (define delta (how 0 amount))
  (define action (dw (align (string-join msg)) date delta))
  (set-account-recents! account (cons action history))
  (list account (write account)))

(define/contract
 deposit
 (configurable-ctc
  (max FIXME/c)
  (types
   (->*
    (account? my-date? string?)
    ()
    #:rest
    (listof string?)
    (action/c account?))))
 (make-d/w +))

(define/contract
 withdraw
 (configurable-ctc
  (max FIXME/c)
  (types
   (->*
    (account? my-date? string?)
    ()
    #:rest
    (listof string?)
    (action/c account?))))
 (make-d/w -))

(define/contract
 (write-check account date amount . purpose)
 (configurable-ctc
  (max FIXME/c)
  (types
   (->*
    (account? my-date? string?)
    ()
    #:rest
    (listof string?)
    (action/c account?))))
 (match-define
  (list the-check bump)
  (create-check account (string-join purpose)))
 (match-define (list account+ do) (withdraw account date amount the-check))
 (define (ackn)
   (printf "Check No. ~a Today: ~a~n" (account-check-no account) (today)))
 (list account+ (λ () (bump) (do) (ackn))))

(define (create-check acc purpose)
  (define x (+ (account-check-no acc) 1))
  (define !
    (if (equal? (string-trim purpose) "")
      (λ () (eprintf "warning: the purpose statement is missing\n"))
      void))
  (list (~a x " " purpose) (lambda () (!) (set-account-check-no! acc x))))

(define/contract
 (show-balance a _date . _others)
 (configurable-ctc
  (max FIXME/c)
  (types (->* (account? my-date?) () #:rest any/c (action/c amount?))))
 (define b (+ (transactions->balance (account-recents a)) (account-balance a)))
 (list b (λ () (printf "Current balance: ~a\n" (amount->decimal-string b)))))

(define/contract
 (update account action-date)
 (configurable-ctc (max FIXME/c) (types (-> account? my-date? account?)))
 (define last-date (account-balance-date account))
 (when (next-month? last-date action-date)
   (define-values
    (balance history)
    (for/fold
     ((balance (account-balance account)) (history (account-history account)))
     ((r (account-recents account)))
     (values (+ (dw-amount r) balance) (cons r history))))
   (set-account-recents! account '())
   (set-account-history! account history)
   (set-account-balance-date! account action-date)
   (set-account-balance! account balance))
 account)

(module+
 test
 (let ()
   (define Acopied (struct-copy account A+100chk))
   (define nextdate '(2026 8 20))
   (define exp
     (account "checking" '() 100 nextdate (account-recents A+100chk) 0))
   (define imperative-result (update Acopied nextdate))
   (check-equal? imperative-result exp "update: check return")
   (check-equal? Acopied exp "update: check effect")))

(define/contract
 (to-html a my-dste #:open (open "open") . _other)
 (configurable-ctc
  (max FIXME/c)
  (types (->* (account? my-date?) () #:rest any/c (action/c any/c))))
 (define page:xml (xexpr->xml (account-to-html a)))
 (define file (name->path (account-name a) #:kind 'html))
 (list
  '_
  (λ ()
    (with-output-to-file
     file
     #:exists
     'replace
     (lambda () (display-xml/content page:xml)))
    (system (~a open " " (path->string file)))
    (sleep 1))))

(module+
 test
 (define (run-new-account/2 name)
   (prompt
    (match-define (list a do) (new-account/2 (λ (x) (control k x)) name))
    (do)
    a))
 (define (run f . others) (match-define (list r _) (apply f others)) r)
 (define (run-for-effect
          f
          a
          #:file-to-be-created
          (fn (name->path (account-name a)))
          .
          others)
   (match-define (list _ do) (apply f a others))
   (dynamic-wind
    (λ () (delete-directory/files fn #:must-exist? #f))
    (λ () (begin0 (with-output-to-string do) (check-true (file-exists? fn))))
    (λ () (sleep 0.1) (delete-file fn)))))

(module+
 test
 (define TTT "ttt")
 (dynamic-wind
  (λ ()
    (define a (run-new-account/2 TTT))
    (check-equal? (run show-balance a (today)) 0)
    (check-equal?
     (with-output-to-string (λ () ((second (show-balance a (today))))))
     "Current balance: 0.00\n"))
  (λ ()
    (check-match
     (with-output-to-string (λ () (check-equal? (run-new-account/2 TTT) 1)))
     (pregexp ALREADY-EXISTS)))
  (λ () (delete-file (name->path TTT)))))

(module+
 test
 (define 2day '(2026 6 6))
 (check-equal?
  (run deposit (struct-copy account Achk) 2day "100" 100purpose)
  A+100chk)
 (check-equal?
  (run withdraw (struct-copy account A+100chk) 2day "50" 50purpose)
  A+100-50chk)
 (let ((a (struct-copy account A+100+25chk)))
   (set-account-check-no! a 0)
   (check-equal?
    (run write-check (struct-copy account A+100chk) 2day "25" 25purpose)
    a)))

(module+
 test
 (check-match
  (run-for-effect
   write-check
   (struct-copy account A+100chk)
   2day
   "25"
   25purpose)
  (pregexp "Check No.")))

(module+
 test
 (let* ((a (struct-copy account A+100chk))
        (f (name->path (account-name a) #:kind "html")))
   (check-equal?
    (run-for-effect
     (λ x (apply to-html/t #:open "ls" x))
     a
     2day
     #:file-to-be-created
     f)
    (~a (path->string f) "\n")))
 (let ((a (struct-copy account A+100chk)))
   (check-equal?
    a
    A+100chk
    "the account remains unmodified due to bad amount")))

(module+
 test
 (check-equal? (run show-balance Achk 2day) 0)
 (check-equal? (run show-balance A+100chk 2day) 100)
 (check-equal? (run show-balance A+100+25chk 2day) 75))

