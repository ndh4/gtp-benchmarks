#lang racket

(require "my-print.rkt")

(provide (struct-out call)
         add-arg-recorder
         execute-call/namespace
         print-and-execute-call)

(define (print-and-execute-call proc the-call)
  (define bugged-call (call (call-proc-name the-call)
                            (call-proc-kws the-call)
                            (map bug-arg (call-kw-args the-call))
                            (map bug-arg (call-pos-args the-call))))
  (begin0
    (execute-call/proc bugged-call proc)
    (my-print bugged-call (current-error-port))
    (eprintf "~n~n")))

(define (execute-call/proc the-call proc)
  (keyword-apply proc
    (call-proc-kws the-call)
    (call-kw-args the-call)
    (call-pos-args the-call)))

(define (execute-call/namespace the-call namespace)
  (execute-call/proc
    the-call
    (eval (call-proc-name the-call) namespace)))

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