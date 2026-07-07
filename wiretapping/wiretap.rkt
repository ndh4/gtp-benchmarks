#lang racket

(require "my-print.rkt")

(provide (struct-out call)
         add-arg-recorder
         print-and-execute-call)

(define (print-and-execute-call proc the-call)
  (define bugged-call (call (call-proc-name the-call)
                            (call-proc-kws the-call)
                            (map bug-arg (call-kw-args the-call))
                            (map bug-arg (call-pos-args the-call))))
  (begin0
    (keyword-apply proc
                   (call-proc-kws bugged-call)
                   (call-kw-args bugged-call)
                   (call-pos-args bugged-call))
    (my-print bugged-call (current-error-port))
    (eprintf "~n~n")))

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
                                         (print-and-execute-call val (call procedure-name '() '() args))))]
                                     [else
                                      (displayln "Contracted value is not a procedure." (current-error-port))
                                      val]))))))

(define (add-arg-recorder c)
  (and/c (arg-recorder) c))