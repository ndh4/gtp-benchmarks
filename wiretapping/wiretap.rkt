#lang racket

(require "./my-print.rkt")

(provide (struct-out call)
         add-arg-recorder
         print-and-execute-call)

(struct call (proc-name proc-kws kw-args pos-args)
  #:prefab)

(define (print-and-execute-call proc the-call)
  (my-print the-call (current-error-port))
  (eprintf "~n~n")
  (define result
    (keyword-apply proc
      (call-proc-kws the-call)
      (call-kw-args the-call)
      (call-pos-args the-call)))
  (eprintf "(result ")
  (my-print result (current-error-port))
  (eprintf ")~n~n")
  result)

(struct arg-recorder ()
  #:property prop:contract
  (build-contract-property
   #:name (λ (c) "arg-recorder")
   #:late-neg-projection (λ (c)
                           (λ (blame)
                             (λ (val neg-party)
                               (cond [(procedure? val)
                                      (define procedure-name (object-name val))
                                      (make-keyword-procedure
                                       (λ (kws kw-args . args)
                                         (print-and-execute-call val (call procedure-name kws kw-args args)))
                                       (λ args
                                         (print-and-execute-call val (call procedure-name '() '() args)))
                                       )]
                                     [else
                                      (displayln "Contracted value is not a procedure." (current-error-port))
                                      val]))))))

(define (add-arg-recorder c)
  (and/c (arg-recorder) c))