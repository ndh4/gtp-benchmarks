#lang racket

(require "structs.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         (only-in "ai.rkt" closed-term?))

(require (only-in
          "ui.rkt"
          format-mono-store
          analyze
          monovariant-store
          monovariant-value
          empty-mono-store
          summarize))

(module+
 test
 (require rackunit (only-in racket/format ~a))
 (define make-fake-gensym
   (lambda ()
     (let ((counter 0))
       (lambda ((x 'g))
         (define new-sym (string->symbol (format "~a~a" x counter)))
         (set! counter (add1 counter))
         new-sym))))
 (define new-label (make-fake-gensym))
 (define (make-ref var) (Ref (new-label) var))
 (define (make-lambda formals call) (Lam (new-label) formals call))
 (define (make-call fun . args) (Call (new-label) fun args))
 (define (make-let var exp call) (make-call (make-lambda (list var) call) exp))
 (define smallest-example (make-lambda '(a) (make-ref 'a)))
 (check-equal? (analyze smallest-example) (hash))
 (define tiny-example
   (make-call
    (make-lambda '(a) (make-ref 'a))
    (make-lambda '(b) (make-ref 'b))))
 (check-equal?
  (analyze tiny-example)
  (hash 'a (set (Lam 'g5 '(b) (Ref 'g4 'b)))))
 (define standard-example
   (make-let
    'id
    (make-lambda '(x k) (make-call (make-ref 'k) (make-ref 'x)))
    (make-call
     (make-ref 'id)
     (make-lambda '(z) (make-ref 'z))
     (make-lambda
      '(a)
      (make-call
       (make-ref 'id)
       (make-lambda '(y) (make-ref 'y))
       (make-lambda '(b) (make-ref 'b)))))))
 (check-equal?
  (analyze standard-example)
  (hash
   'a
   (set (Lam 'g13 '(z) (Ref 'g12 'z)))
   'b
   (set (Lam 'g16 '(y) (Ref 'g15 'y)))
   'id
   (set (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x)))))
   'k
   (set
    (Lam
     'g20
     '(a)
     (Call
      'g19
      (Ref 'g14 'id)
      (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
    (Lam 'g18 '(b) (Ref 'g17 'b)))
   'x
   (set (Lam 'g13 '(z) (Ref 'g12 'z)) (Lam 'g16 '(y) (Ref 'g15 'y))))))

