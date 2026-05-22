#lang racket

(provide (struct-out call)
         add-arg-recorder)

(struct call (proc-name proc-kws kw-args pos-args)
  #:transparent)

(struct arg-recorder ()
  #:property prop:contract
  (build-contract-property
   #:name (λ (c) "arg-recorder")
   #:late-neg-projection (λ (c)
                           (λ (blame)
                             (λ (val neg-party)
                               (cond [(procedure? val)
                                      (define procedure-name (object-name val))
                                      (define (print-and-execute-call proc the-call)
                                        (printf "~v~n" the-call)
                                        (keyword-apply proc
                                                       (call-proc-kws the-call)
                                                       (call-kw-args the-call)
                                                       (call-pos-args the-call)))
                                      (make-keyword-procedure
                                       (λ (kws kw-args . args)
                                         (print-and-execute-call val (call procedure-name kws kw-args args)))
                                       (λ args
                                         (print-and-execute-call val (call procedure-name '() '() args)))
                                       )]
                                     [else
                                      (displayln 'b)
                                      val]))))))

(define (add-arg-recorder c)
  (and/c (arg-recorder) c))