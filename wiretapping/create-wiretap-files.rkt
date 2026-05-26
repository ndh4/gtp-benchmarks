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

(define (create-wiretap-files-whole-benchmark benchmark)
  (define base-dir (build-path path-to-bench-dirs benchmark))
  (define input-dir (build-path base-dir "original"))
  (define output-dir (build-path base-dir "wiretap"))
  (unless (directory-exists? output-dir) (make-directory output-dir))
  (for ([in-file (directory-list input-dir #:build? #t)])
    (when (file-exists? in-file)
      (copy-file in-file (build-path output-dir (file-name-from-path in-file)) #:exists-ok? #t)
      (create-wiretap-files-one-module #:in-file in-file #:out-dir output-dir))))

(define (create-wiretap-files-one-module #:in-file in-file #:out-dir out-dir)
  (define contracted-identifiers (find-contracted-identifiers in-file))
  (for ([identifier contracted-identifiers])
    (create-wiretap-file #:identifier identifier #:in-file in-file #:out-dir out-dir)))


(define (find-contracted-identifiers file)
  (call-with-input-file file
    (lambda (in)
      (read-line in) ;; Discard hashlang declaration
      (let loop ([accum '()])
        (define expr (read in))
        (if (eof-object? expr)
            accum
            (match expr
              [(list* 'define/contract (cons (? symbol? id) _) _ _)
               ;; ^ Match shorthand function definitions: (define (id args ...) body ...)
               (loop (cons id accum))]
              [else
               (when (string-contains? (~a expr) "define/contract")
                 (printf "!Possible missed contract(s): ~a~n~n" expr))
               (loop accum)]))))))


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
          ;; Replace #lang with racket because we need 'quote to specify contract level
          (read-line in)
          (displayln "#lang racket" out)
          (newline out)
          
          (pretty-write `(define-syntax ctc-level ',contract-level) out)
          (pretty-write `(require "../../../wiretapping/wiretap.rkt") out)
          (newline out)
          (let loop ()
            (define expr (read in))
            (cond [(eof-object? expr)
                   (pretty-write (make-exercise identifier) out)]
                  [else
                   (match expr
                     [(list* 'define/contract (cons (== identifier) args) ctc body)
                      (pretty-write
                       `(define/contract ,(cons identifier args) (add-arg-recorder ,ctc) ,@body) out)]
                     [(list* 'module+ 'test _)
                      (void)]
                     [_
                      (pretty-write expr out)])
                   (newline out)
                   (loop)])))))))

(define (make-exercise identifier)
  `(for ([fuel (in-range 10)])
     (contract-exercise ,identifier #:fuel fuel)))

(define contract-level 'max)

(for ([benchmark benchmarks])
  (create-wiretap-files-whole-benchmark benchmark))