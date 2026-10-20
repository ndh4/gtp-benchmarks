#lang racket

;; WARNING: I used AI to help write this file. I believe I've fixed all the bugs, but you never know. -nh

(require racket/pretty)

(define (transform-contracts in-file out-file)
  (define contract-map (make-hash))
  ;; Keep track of struct contracts specifically to rebuild struct/contract forms later
  (define struct-contract-map (make-hash))
  
  (call-with-input-file in-file
    (lambda (in)
      (call-with-output-file out-file
        (lambda (out)
          (displayln (read-line in) out) ;;hashlang
          (newline out)
          (let loop ()
            (define expr (read in))
            (if (eof-object? expr)
                ;; End of file reached: check if any contracts were left unused
                (unless (hash-empty? contract-map)
                  (for ([missing-id (in-hash-keys contract-map)])
                    (eprintf "WARNING: A contract was provided for '~a', but no corresponding definition was found in the file.\n" missing-id)))
                
                ;; Continue processing expressions
                (begin
                  (match expr
                    ;; 0.5. Special case
                    ['(define-syntax-rule (define-wall name single-bar double-bar)
                        (begin (define name
                                 (class wall%
                                   (inspect #f)
                                   (define/override (show) (if double-bar? double-bar single-bar))
                                   (super-new)))
                               ;; parse either kind
                               (register-cell-type! name single-bar)
                               (register-cell-type! name double-bar)
                               (provide/configurable-contract [name ([max (make-cell%/c-with self
                                                                                             (if double-bar? double-bar single-bar)
                                                                                             equal?)]
                                               [types cell%/c])])))

                     (define dsr-expr '(define-syntax-rule
                                         (define-wall name single-bar double-bar)
                                         (begin
                                           (define/contract name
                                             (configurable-ctc
                                              (max
                                               (make-cell%/c-with self (if double-bar? double-bar single-bar) equal?))
                                              (types cell%/c))
                                             (class wall%
                                               (inspect #f)
                                               (define/override (show) (if double-bar? double-bar single-bar))
                                               (super-new)))
                                           (register-cell-type! name single-bar)
                                           (register-cell-type! name double-bar)
                                           (provide name))))
                     (pretty-write dsr-expr out)
                     (newline out)]
                    
                    ;; 1. Match the provide/configurable-contract block
                    [(list 'provide/configurable-contract items ...)
                     (define provide-items
                       (for/list ([item items])
                         (match item
                           ;; Match a struct specification block
                           [(list 'struct struct-id (list (list fields field-ctcs) ...))
                            ;; Save the complete fields mapping to reconstruct the struct/contract later
                            (define field-map (for/hash ([f fields] [c field-ctcs]) (values f c)))
                            (hash-set! struct-contract-map struct-id field-map)

                            ;; Register expected runtime identifiers in the map so warnings check out
                            (hash-set! contract-map struct-id field-ctcs)

                            ;; Standard provide output for the struct name
                           `(struct-out ,struct-id)]

                           ;; Match standard identifier contracts: [id ctc]
                           [(list id ctc)
                            (hash-set! contract-map id ctc)
                            id])))

                     ;; Generate and write the standard provide block
                     (define provide-expr `(provide ,@provide-items))
                     (pretty-write provide-expr out)
                     (newline out)]

                    [(list 'require/configurable-contract modname ids ...)
                     
                     (define require-expr `(require (only-in ,modname ,@ids)))
                     (pretty-write require-expr out)
                     (newline out)]

                    [(list* 'struct (? symbol? struct-id) (list fields ...) options)
                     (cond [(hash-has-key? struct-contract-map struct-id)
                            (define field-map (hash-ref struct-contract-map struct-id))
                            ;; Rebuild fields paired with their specific configurable-ctc wrapper
                            (define contract-fields
                              (for/list ([f fields])
                                (define f-ctc (hash-ref field-map f '(([missing FIXME/c])))) ; default fallback if missing
                                `[,f (configurable-ctc ,@f-ctc)]))

                            ;; Emits struct/contract syntax preserving all structural options/attributes
                            (define transformed-struct `(struct/contract ,struct-id ,contract-fields ,@options))
                            (pretty-write transformed-struct out)
                            (hash-remove! contract-map struct-id)
                            (hash-remove! struct-contract-map struct-id)]
                           [else (pretty-write expr out)])
                     (newline out)]
                    
                    ;; 2a. Match shorthand function definitions: (define (id args ...) body ...)
                    [(list* 'define (cons (? symbol? id) args) body)
                     (cond
                       [(hash-has-key? contract-map id)
                        (define ctc (hash-ref contract-map id))
                        (define transformed-expr
                          `(define/contract (,id ,@args)
                             (configurable-ctc ,@ctc)
                             ,@body))
                        (pretty-write transformed-expr out)
                        ;; Remove the ID from the map to mark it as found
                        (hash-remove! contract-map id)]
                       
                       [else
                        (pretty-write expr out)])
                     (newline out)]
                    
                    ;; 2b. Match plain variable definitions: (define id value)
                    [(list 'define (? symbol? id) val)
                     (cond
                       [(hash-has-key? contract-map id)
                        (define ctc (hash-ref contract-map id))
                        (define transformed-expr
                          `(define/contract ,id
                             (configurable-ctc ,@ctc)
                             ,val))
                        (pretty-write transformed-expr out)
                        ;; Remove the ID from the map to mark it as found
                        (hash-remove! contract-map id)]
                       
                       [else
                        (pretty-write expr out)])
                     (newline out)]
                    
                    ;; 3. Match everything else and pass it through unchanged
                    [_
                     (pretty-write expr out)
                     (newline out)])
                  
                  (loop)))))
        #:exists 'replace))))


(define args (current-command-line-arguments))

(define bmname "xmanage")
(define fnames '("actions" "data" "date" "file-io" "html"))

;; Example usage:
(for ([fname fnames])
  (transform-contracts (format "/Users/nhejduk/Research-Local/teco-parent/gtp-benchmarks/benchmarks/~a/original-providectc/~a.rkt" bmname fname) (format "/Users/nhejduk/Research-Local/teco-parent/gtp-benchmarks/benchmarks/~a/original/~a.rkt" bmname fname))
)