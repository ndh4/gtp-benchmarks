#lang racket

;; WARNING: I used AI to help write this file. I believe I've fixed all the bugs, but you never know. -nh

(require racket/pretty)

(define (transform-contracts in-file out-file)
  (define contract-map (make-hash))
  
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
                    
                    ;; 1. Match the provide/configurable-contract block
                    [(list 'provide/configurable-contract (list ids ctcs) ...)
                     (for ([id ids] [ctc ctcs])
                       (hash-set! contract-map id ctc))
                     
                     ;; Generate and write a standard provide block
                     (define provide-expr `(provide ,@ids))
                     (pretty-write provide-expr out)
                     (newline out)]

                    [(list 'require/configurable-contract modname ids ...)
                     
                     (define require-expr `(require (only-in ,modname ,@ids)))
                     (pretty-write require-expr out)
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


(define bmname "kcfa")
(define fname "time")

;; Example usage:
(transform-contracts (format "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks/~a/original/~a.rkt" bmname fname) (format "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks/~a/original-definectc/~a.rkt" bmname fname))