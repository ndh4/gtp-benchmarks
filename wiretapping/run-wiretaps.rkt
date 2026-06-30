#lang racket

(require "params.rkt"
         racket/sandbox)

(define (run-wiretap-on-benchmark #:benchmark benchmark #:contract-level contract-level)
  (define base-dir (build-path path-to-bench-dirs benchmark))
  (define wiretap-dir (build-path base-dir (format "wiretap-~a" contract-level)))
  (define output-dir (build-path base-dir (format "wiretap-~a-results" contract-level)))
  (unless (directory-exists? output-dir)
    (make-directory output-dir))
  (for ([wiretap-runner (directory-list wiretap-dir #:build? #t)]
        #:when (and (file-exists? wiretap-runner)
                    (equal? (path-get-extension wiretap-runner) #".rkt")
                    (string-contains? (path->string (file-name-from-path wiretap-runner)) "_TAP_")))
      (run-one-wiretap #:run-dir wiretap-dir #:runner wiretap-runner #:out-dir output-dir)))

(define (run-one-wiretap #:run-dir run-dir #:runner wiretap-runner #:out-dir output-dir)
  (define output-file (build-path output-dir (path-replace-extension (file-name-from-path wiretap-runner) ".rktd")))
  (call-with-output-file output-file #:exists 'replace
    (lambda (out)
      (define inspector (current-code-inspector))
      (parameterize ([current-output-port out]
                     [current-directory run-dir]
                     [sandbox-error-output out]
                     [sandbox-make-code-inspector (thunk inspector)]
                     [sandbox-security-guard (current-security-guard)]  ; Allow any file/network access
                     [sandbox-eval-limits (list TIME_LIMIT_SECONDS MEMORY_LIMIT_MB)]
                     [sandbox-propagate-exceptions #t])     ; Pass all errors through
        (with-handlers ([exn:fail? (lambda (e) (printf "[Reached error~n-------------~n~a]~n" (exn-message e)))])
          (define eval (make-module-evaluator wiretap-runner))
          (eval (make-base-namespace)))))))

(for* ([contract-level contract-levels]
       [benchmark benchmarks])
  (printf "Running wiretap on \"~a\" with contract level \"~a\", ~as time limit, and ~aMB memory limit...~n"
           benchmark contract-level TIME_LIMIT_SECONDS MEMORY_LIMIT_MB)
  (run-wiretap-on-benchmark #:benchmark benchmark #:contract-level contract-level))

#;(run-one-wiretap #:run-dir "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks/kcfa/wiretap-max"
                 #:runner (string->path "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks/kcfa/wiretap-max/ai_TAP_explore.rkt")
                 #:out-dir "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/wiretap-scratch-results")