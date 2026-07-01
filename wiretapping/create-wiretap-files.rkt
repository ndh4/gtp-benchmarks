#lang racket

(require "params.rkt")

(define (create-wiretap-files-whole-benchmark #:benchmark benchmark #:contract-level contract-level)
  (define base-dir (build-path path-to-bench-dirs benchmark))
  (define input-dir (build-path base-dir "original"))
  (define output-dir (build-path base-dir (format "wiretap-~a" contract-level)))
  (unless (directory-exists? output-dir) (make-directory output-dir))
  (for ([in-file (directory-list input-dir #:build? #t)]
        #:when (and (file-exists? in-file)
                    (equal? (path-get-extension in-file) #".rkt")))
    (add-ctc-level #:in-file in-file #:out-file (build-path output-dir (file-name-from-path in-file)) #:ctc-level contract-level)
    (create-wiretap-files-one-module #:in-file in-file #:out-dir output-dir #:ctc-level contract-level)))

(define (add-ctc-level #:in-file in-file #:out-file out-file #:ctc-level contract-level)
  (call-with-input-file in-file
    (lambda (in)
      (call-with-output-file out-file #:exists 'replace
        (lambda (out)
          (rw-hashlang-and-ctc-level #:in in #:out out #:ctc-level contract-level)
          (reader-loop
           #:in in
           #:initial-accum-val '()
           #:on-expr
           (lambda (#:expr expr #:accum accum)
             (newline out)
             (pretty-write expr out)
             '())))))))

(define (reader-loop #:in in #:initial-accum-val initial-accum-val #:on-expr on-expr)
  (let loop ([accum initial-accum-val])
    (define expr (read in))
    (cond [(eof-object? expr)
           accum]
          [else
           (loop (on-expr #:expr expr #:accum accum))])))

(define (rw-hashlang-and-ctc-level #:in in #:out out #:ctc-level contract-level)
  (displayln (read-line in) out)
  (newline out)
  (pretty-write '(require (for-syntax racket/base)) out)
  (pretty-write `(define-syntax ctc-level ',contract-level) out))

(define (create-wiretap-files-one-module #:in-file in-file #:out-dir out-dir #:ctc-level contract-level)
  (define contracted-identifiers (find-contracted-identifiers in-file))
  (for ([identifier contracted-identifiers])
    (create-wiretap-file #:identifier identifier #:in-file in-file #:out-dir out-dir #:ctc-level contract-level)))

(define (find-contracted-identifiers file)
  (call-with-input-file file
    (lambda (in)
      (read-line in) ;; Ignore hashlang declaration
      (reader-loop
       #:in in
       #:initial-accum-val '()
       #:on-expr
       (lambda (#:expr expr #:accum accum)
         (match expr
           [(list* 'define/contract (cons (? symbol? id) _) _ _)
            ;; ^ Match shorthand function definitions: (define (id args ...) body ...)
            (cons id accum)]
           [else
            (when (string-contains? (~a expr) "define/contract")
              (printf "!Possible missed contract(s): ~a~n~n" expr))
            accum]))))))


(define (sanitize identifier)
  (for/fold ([str (~a identifier)])
            ([replacer (in-list '(("/" . "_") ("!" . "B") ("?" . "H")))])
    (string-replace str (car replacer) (cdr replacer))))

(define (create-wiretap-file #:identifier identifier #:in-file in-file #:out-dir out-dir #:ctc-level contract-level)
  (define out-file
    (build-path out-dir
                (format "~a_TAP_~a.rkt"
                        (path-replace-extension (file-name-from-path in-file) "")
                        (sanitize identifier))))
  (call-with-input-file in-file
    (lambda (in)
      (call-with-output-file out-file #:exists 'replace
        (lambda (out)
          (rw-hashlang-and-ctc-level #:in in #:out out #:ctc-level contract-level)
          (newline out)
          (pretty-write `(require "../../../wiretapping/wiretap.rkt") out)
          (reader-loop
           #:in in
           #:initial-accum-val '()
           #:on-expr
           (lambda (#:expr expr #:accum accum)
             (newline out)
             (match expr
               [(list* 'define/contract (cons (== identifier) args) ctc body)
                (pretty-write
                 `(define/contract ,(cons identifier args) (add-arg-recorder ,ctc) ,@body) out)]
               [(list* 'module+ 'test _)
                (void)]
               [_
                (pretty-write expr out)])
             '()))
          (newline out)
          (pretty-write (make-exercise identifier) out))))))

(define (make-exercise identifier)
  `(for ([fuel (in-range 10)])
     (contract-exercise ,identifier #:fuel fuel)))

(for* ([contract-level contract-levels]
       [benchmark benchmarks])
  (create-wiretap-files-whole-benchmark #:benchmark benchmark #:contract-level contract-level))