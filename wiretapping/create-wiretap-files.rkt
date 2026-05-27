#lang racket

(define path-to-bench-dirs
  "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks")

(define benchmarks
  '("mbta"
    "morsecode"
    "sieve"
    "snake"
    "kcfa"
    "dungeon"
    "forth")
  )

(define contract-level 'max)

(define (create-wiretap-files-whole-benchmark benchmark)
  (define base-dir (build-path path-to-bench-dirs benchmark))
  (define input-dir (build-path base-dir "original"))
  (define output-dir (build-path base-dir (format "wiretap-~a" contract-level)))
  (unless (directory-exists? output-dir) (make-directory output-dir))
  (for ([in-file (directory-list input-dir #:build? #t)])
    (when (file-exists? in-file)
      (add-ctc-level #:in-file in-file #:out-file (build-path output-dir (file-name-from-path in-file)))
      (create-wiretap-files-one-module #:in-file in-file #:out-dir output-dir))))

(define (add-ctc-level #:in-file in-file #:out-file out-file)
  (call-with-input-file in-file
    (lambda (in)
      (call-with-output-file out-file #:exists 'replace
        (lambda (out)
          (rw-hashlang-and-ctc-level #:in in #:out out)
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

(define (rw-hashlang-and-ctc-level #:in in #:out out)
  (displayln (read-line in) out)
  (newline out)
  (pretty-write '(require (for-syntax racket/base)) out)
  (pretty-write `(define-syntax ctc-level ',contract-level) out))

(define (create-wiretap-files-one-module #:in-file in-file #:out-dir out-dir)
  (define contracted-identifiers (find-contracted-identifiers in-file))
  (for ([identifier contracted-identifiers])
    (create-wiretap-file #:identifier identifier #:in-file in-file #:out-dir out-dir)))

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
  (string-replace (~a identifier) "/" "_"))

(define (create-wiretap-file #:identifier identifier #:in-file in-file #:out-dir out-dir)
  (define out-file
    (build-path out-dir
                (format "~a_TAP_~a.rkt"
                        (path-replace-extension (file-name-from-path in-file) "")
                        (sanitize identifier))))
  (call-with-input-file in-file
    (lambda (in)
      (call-with-output-file out-file #:exists 'replace
        (lambda (out)
          (rw-hashlang-and-ctc-level #:in in #:out out)
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

(for ([benchmark benchmarks])
  (create-wiretap-files-whole-benchmark benchmark))