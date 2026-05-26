#lang racket

(require racket/sandbox)

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


(define (run-wiretap-on-benchmark benchmark)
  (define base-dir (build-path path-to-bench-dirs benchmark))
  (define wiretap-dir (build-path base-dir "wiretap"))
  (define output-dir (build-path base-dir "wiretap-results"))
  (unless (directory-exists? output-dir)
    (make-directory output-dir))
  (for ([wiretap-runner (directory-list wiretap-dir #:build? #t)]
        #:when (and (file-exists? wiretap-runner)
                    (string-contains? (path->string (file-name-from-path wiretap-runner)) "_TAP_")))
      (run-one-wiretap #:run-dir wiretap-dir #:runner wiretap-runner #:out-dir output-dir)))

(define (run-one-wiretap #:run-dir run-dir #:runner wiretap-runner #:out-dir output-dir)
  (define output-file (build-path output-dir (path-replace-extension (file-name-from-path wiretap-runner) ".rktd")))
  (call-with-output-file output-file #:exists 'replace
    (lambda (out)
      (define inspector (current-code-inspector))
      (parameterize ([current-output-port out]
                     [current-directory run-dir]
                     [sandbox-memory-limit 50]
                     [sandbox-output out]
                     [sandbox-make-code-inspector (thunk inspector)]
                     [sandbox-security-guard (current-security-guard)]  ; Allow any file/network access
                     [sandbox-eval-limits #f]               ; No time/CPU limits
                     [sandbox-propagate-exceptions #t])     ; Pass all errors through
        (with-handlers ([exn:fail? (lambda (e) (printf "[Reached error~n-------------~n~a]~n" (exn-message e)))])
          (define eval (make-module-evaluator wiretap-runner))
          (eval (make-base-namespace)))))))

(for ([benchmark benchmarks])
  (run-wiretap-on-benchmark benchmark))