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

;; --- ALL COMPILE-TIME STATE, HELPERS, AND CLASSES UNIFIED IN PHASE 1 ---
(begin-for-syntax
  (define files-with-provide-configurable (mutable-set))
  (define enabled-ctc-levels '(none types max))
  
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
                          'none)))

  (define-syntax-class ctc-map-clause
    #:attributes [level ctc]
    (pattern [level:id ctc:expr]))

  (define-syntax-class ctc-map
    #:attributes [raw-hash]
    (pattern {clause:ctc-map-clause ...}
             #:attr raw-hash (hash-set (for/hash ([lvl (in-list (attribute clause.level))]
                                                  [ctc (in-list (attribute clause.ctc))])
                                         (values (syntax->datum lvl) ctc))
                                       'none #'any/c)))

  (define-syntax-class struct-field-clause
    #:attributes [field raw-hash]
    (pattern [field:id cmap:ctc-map]
             #:attr raw-hash (attribute cmap.raw-hash)))

  (define-syntax-class provision-spec
    #:attributes [export-builder lowered-maps-syntax]
    ;; Normal values or functions
    (pattern [name:id cmap:ctc-map]
             #:attr export-builder
             (λ (level) #`(provide (contract-out [name #,(hash-ref (attribute cmap.raw-hash) level)])))
             #:attr lowered-maps-syntax
             (λ (ctx) (list #`'name ((lower-ctc-map ctx) (attribute cmap.raw-hash)))))
  
    ;; Struct definitions
    (pattern [struct struct-id:id (fields:struct-field-clause ...)]
             #:attr export-builder
             (λ (level)
               (if (eq? level 'none)
                   #`(provide (struct-out struct-id))
                   #`(provide (contract-out [struct struct-id (#,@(for/list ([f-hash (in-list (attribute fields.raw-hash))])
                                                                    (hash-ref f-hash level)))]))))
             #:attr lowered-maps-syntax
             (λ (ctx)
               (append* 
                (for/list ([f-id (in-list (attribute fields.field))]
                           [f-hash (in-list (attribute fields.raw-hash))])
                  (define accessor-name (format-id ctx "~a-~a" #'struct-id f-id))
                  (list #`'#,accessor-name ((lower-ctc-map ctx) f-hash))))))))

;; --- TRANSFORMERS ---
(define-syntax (provide/configurable-contract stx)
  (syntax-parse stx
    [(_ spec:provision-spec ...)
     #:do [(define current-level (contract-level-for-expansion-context-of this-syntax))
           (define filename (syntax-source stx))
           (define first-provide-in-this-module? (not (set-member? files-with-provide-configurable filename)))
           (when first-provide-in-this-module?
             (set-add! files-with-provide-configurable filename))]
     
     #:with (compiled-export ...) (for/list ([builder (in-list (attribute spec.export-builder))])
                                    (builder current-level))
     
     #:with [flattened-map-entries ...] (append* (for/list ([lower-func (in-list (attribute spec.lowered-maps-syntax))])
                                                    (lower-func #'here)))
     #:with syntax-for-contract-maps-id (datum->syntax this-syntax 'contract-maps this-syntax this-syntax)
     #:with [maybe-define/provide-contract-maps ...] (if first-provide-in-this-module?
                                                          #'{(provide syntax-for-contract-maps-id)
                                                             (define syntax-for-contract-maps-id (make-hash))}
                                                          #'{})
     #'(begin
         compiled-export ...
         (module+ syntax-for-contract-maps-id
           maybe-define/provide-contract-maps ...
           (hash-set*! syntax-for-contract-maps-id flattened-map-entries ...)))]))

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
