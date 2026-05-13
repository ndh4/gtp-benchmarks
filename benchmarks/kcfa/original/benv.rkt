#lang racket

(require "structs.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt")

(provide empty-benv benv-lookup benv-extend benv-extend*)

(provide (struct-out Closure)
         (struct-out Binding)
         BEnv?
         Closure/c
         Binding/c
         Closure-type/c
         Binding-type/c
         Time?
         Addr?
         Closure-lam
         Closure-benv
         Binding-var
         Binding-time)

(struct Closure (lam benv) #:mutable #:transparent)

(struct Binding (var time) #:mutable #:transparent)

(define/ctc-helper (Closure/c lam/c benv/c) (struct/c Closure lam/c benv/c))

(define/ctc-helper (Binding/c var/c time/c) (struct/c Binding var/c time/c))

(define/ctc-helper Time? (listof symbol?))

(define/ctc-helper Binding-type/c (Binding/c Var? Time?))

(define/ctc-helper Addr? Binding-type/c)

(define/ctc-helper BEnv? (hash/c Var? Addr? #:immutable #t))

(define/ctc-helper Closure-type/c (Closure/c Lam-type/c BEnv?))

(define/contract
 empty-benv
 (configurable-ctc (max BEnv?) (types BEnv?))
 (hash))

(define/ctc-helper ((key-of/c a-hash) k) (hash-has-key? a-hash k))

(define/contract
 benv-lookup
 (configurable-ctc
  (max
   (->i
    ((benv BEnv?) (key (benv) (and/c Var? (key-of/c benv))))
    (result (benv key) (equal?/c (hash-ref benv key)))))
  (types (-> BEnv? Var? Addr?)))
 hash-ref)

(define/contract
 benv-extend
 (configurable-ctc
  (max
   (->i
    ((benv BEnv?) (key Var?) (val Addr?))
    (result BEnv?)
    #:post
    (benv key val result)
    (and (hash-has-key? result key) (equal? (hash-ref result key) val))))
  (types (-> BEnv? Var? Addr? BEnv?)))
 hash-set)

(define/contract
 (benv-extend* benv vars addrs)
 (configurable-ctc
  (max
   (->i
    ((benv BEnv?) (keys (listof Var?)) (vals (listof Addr?)))
    (result BEnv?)
    #:post
    (benv keys vals result)
    (for/and
     ((k (in-list keys)) (v (in-list vals)))
     (and (hash-has-key? result k) (equal? (hash-ref result k) v)))))
  (types (-> BEnv? (listof Var?) (listof Addr?) BEnv?)))
 (for/fold
  ((benv benv))
  ((v (in-list vars)) (a (in-list addrs)))
  (benv-extend benv v a)))

(module+
 test
 (require rackunit (only-in racket/format ~a))
 (define benv1 (benv-extend empty-benv 'x (Binding 'x '(g123))))
 (check-equal? (benv-lookup benv1 'x) (Binding 'x '(g123)))
 (define benv2
   (benv-extend* benv1 '(x k) (list (Binding 'x '(g19)) (Binding 'k '(g19)))))
 (check-equal? (benv-lookup benv2 'k) (Binding 'k '(g19)))
 (check-equal? (benv-lookup benv2 'x) (Binding 'x '(g19))))

