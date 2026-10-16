#lang at-exp racket

(provide provide/configurable-contract
         require/configurable-contract)

(require syntax/parse/define
         racket/contract
         (for-syntax syntax/parse
                     racket/format
                     racket/list
                     racket/set
                     racket/syntax))

(begin-for-syntax
  (define files-with-provide-configurable (mutable-set))
  (define enabled-ctc-levels '(none types max))

  (define-syntax-class ctc-map-clause
    #:attributes [level ctc]
    (pattern [level:id ctc:expr]
             #:when (member (syntax->datum #'level) enabled-ctc-levels)))

  (define-syntax-class ctc-map
    #:attributes [[clause 1] map]
    (pattern {clause:ctc-map-clause ...}
             ;; ctc-map? := (hash/c id? syntax?)
             #:attr map (hash-set (for/hash ([level (in-list (attribute clause.level))]
                                             [ctc (in-list (attribute clause.ctc))])
                                    (values (syntax->datum level) ctc))
                                  'none #'any/c)))

  (define-logger configurable-ctcs)

  (define ((lower-ctc-map ctx) m)
    #`(hash #,@(append* (for/list ([{level ctc-stx} (in-hash m)])
                          (list #`'#,level ctc-stx)))))

  (define (contract-level-for-expansion-context-of stx)
    (syntax-local-value (datum->syntax stx 'ctc-level)
                        (λ _
                          (log-configurable-ctcs-warning
                           @~a{no ctc level configured for mod @;
                               @(syntax-source stx), @;
                               defaulting to none})
                          'none))))

(define-syntax (provide/configurable-contract stx)
  (syntax-parse stx
    [(_ [name:id cmap:ctc-map] ...)
     #:do [(define current-level (contract-level-for-expansion-context-of this-syntax))
           (define filename (syntax-source stx))
           (define first-provide-in-this-module? (not (set-member? files-with-provide-configurable filename)))
           (when first-provide-in-this-module?
             (set-add! files-with-provide-configurable filename))]
     #:with [ctc ...] (for/list ([m (in-list (attribute cmap.map))])
                        (hash-ref m current-level))
     #:with [ctc-map-stx ...] (map (lower-ctc-map #'here) (attribute cmap.map))
     #:with syntax-for-contract-maps-id (datum->syntax this-syntax 'contract-maps this-syntax this-syntax)
     #:with [maybe-define/provide-contract-maps ...] (if first-provide-in-this-module?
                                                          #'{(provide syntax-for-contract-maps-id)
                                                             (define syntax-for-contract-maps-id (make-hash))}
                                                          #'{})
     #'(begin
         (provide (contract-out [name ctc] ...))
         (module+ syntax-for-contract-maps-id
           maybe-define/provide-contract-maps ...
           (hash-set*! syntax-for-contract-maps-id {~@ 'name ctc-map-stx} ...)))]))

(define-simple-macro (require/configurable-contract mod:str name:id ...)
  #:do [(define current-level (contract-level-for-expansion-context-of this-syntax))]
  #:with current-level-stx #`'#,current-level
  #:with middle-mod-name (format-id #f "middleman-~a" (string->symbol (syntax->datum #'mod)))
  #:with syntax-to-require-middle-mod (datum->syntax this-syntax
                                                     (list 'require (list 'quote (format-id #f "middleman-~a" (string->symbol (syntax->datum #'mod)))))
                                                     this-syntax
                                                     this-syntax)
  (begin
    (module middle-mod-name racket/base
      (require racket/contract
               mod
               (submod mod contract-maps))
      (provide (contract-out [name (hash-ref (hash-ref contract-maps 'name) current-level-stx)] ...)))
    syntax-to-require-middle-mod))
