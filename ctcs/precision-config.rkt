#lang racket/base

(require (for-syntax syntax/parse
                     racket/list
                     racket/base
                     (only-in racket/function
                              curryr)
                     racket/runtime-path
                     racket/file)
         racket/contract
         (only-in racket/function curry)
         (only-in racket/match match))

(provide configurable-ctc
         define/ctc-helper
         define-syntax/ctc-helper
         define-syntax-rule/ctc-helper)

(begin-for-syntax
  (define known-precision-configs '(none types max/sub1 max))
  (define-logger configurable-ctcs)
  (define (contract-level-for-expansion-context-of stx)
    (syntax-local-value (datum->syntax stx 'ctc-level)
                        (λ _
                          (log-configurable-ctcs-warning
                          (format "no ctc level configured for mod @(syntax-source stx), defaulting to none"
                            (syntax-source stx)))
                          'none))))

;; Must be a member of known-precision-configs; modify to change all configurable-ctc precision levels

;; Usage:
;; (configurable-ctc [<unquoted-precision-config> contract?] ...)
;;
;; No need to specify the 'none case: any unspecified cases will
;; become any/c when that configuration is used.
;;
;; Example:
;; (define/contract (f x)
;;   (configurable-ctc [max (-> number? string?)]
;;                     [types procedure?]))
(define-syntax (configurable-ctc stx)
  (syntax-parse stx
    [(_ [level ctc] ...)
    #:do [(define current-level (contract-level-for-expansion-context-of this-syntax))]
     (let* ([levels (syntax->datum #'(level ...))]
            [ctcs (flatten (syntax->list #'(ctc ...)))]
            [current-level-index (index-of levels
                                           current-level)]
            [current-ctc-stx
             (cond [(number? current-level-index)
                    (list-ref ctcs current-level-index)]
                   ;; none wasn't really intended to be specified
                   [(equal? current-level 'none)
                    #'any/c]
                   [else
                    (error 'configurable-ctc
                           "Contract fails to specify current module precision: ~a, ~a, ~a"
                           levels ctcs stx)])])
       (begin
         (unless (andmap (curryr member known-precision-configs) levels)
           (error 'configurable-ctc
                  "Unknown precision config provided at ~a" stx))
         (with-syntax ([ctc-for-current-level current-ctc-stx])
           (syntax/loc current-ctc-stx
             ctc-for-current-level))))]))

(define-syntax (define/ctc-helper stx)
  (syntax-parse stx
    [(_ e ...)
     (syntax/loc stx (define e ...))]))
(define-syntax (define-syntax/ctc-helper stx)
  (syntax-parse stx
    [(_ e ...)
     (syntax/loc stx (define-syntax e ...))]))
(define-syntax (define-syntax-rule/ctc-helper stx)
  (syntax-parse stx
    [(_ e ...)
     (syntax/loc stx (define-syntax-rule e ...))]))

