#lang racket
;; Binding environment,
;; helper functions

(require
  "structs.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  "../../../ctcs/configurable.rkt"
)

(provide/configurable-contract
 [empty-benv ([max BEnv?]
              [types BEnv?])]
 [benv-lookup ([max (->i ([benv BEnv?]
                          [key (benv) (and/c Var?
                                             (key-of/c benv))])
                         [result (benv key)
                                 (equal?/c (hash-ref benv key))])]
               [types (BEnv? Var? . -> . Addr?)])]
 [benv-extend ([max (->i ([benv BEnv?]
                          [key Var?]
                          [val Addr?])
                         [result BEnv?]
                         #:post (benv key val result)
                         (and (hash-has-key? result key)
                              (equal? (hash-ref result key) val)))]
               [types (BEnv? Var? Addr? . -> . BEnv?)])]
 [benv-extend* ([max (->i ([benv BEnv?]
                           [keys (listof Var?)]
                           [vals (listof Addr?)])
                          [result BEnv?]
                          #:post (benv keys vals result)
                          (for/and ([k (in-list keys)]
                                    [v (in-list vals)])
                            (and (hash-has-key? result k)
                                 (equal? (hash-ref result k) v))))]
                [types (BEnv? (listof Var?) (listof Addr?) . -> . BEnv?)])])

(provide
  (struct-out Closure)
  (struct-out Binding)
;;   empty-benv
;;   benv-lookup
;;   benv-extend
;;   benv-extend*

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
  Binding-time
)

;; =============================================================================

;; -- private

;(define-type BEnv (HashTable Var Addr))
;(define-type Addr Binding)
;(define-type Time (Listof Label))

;; -- structs

(struct Closure
 (lam ;: Lam]
  benv ;: BEnv]))
)
  #:mutable
  #:transparent)
(struct Binding
 (var ;: Var]
  time ;: Time]))
)
  #:mutable
  #:transparent)

(define/ctc-helper (Closure/c lam/c benv/c)
  (struct/c Closure lam/c benv/c))
(define/ctc-helper (Binding/c var/c time/c)
  (struct/c Binding var/c time/c))

(define/ctc-helper Time? (listof symbol?))

(define/ctc-helper Binding-type/c (Binding/c Var? Time?))
(define/ctc-helper Addr? Binding-type/c)


(define/ctc-helper BEnv? (hash/c Var? Addr? #:immutable #t))

(define/ctc-helper Closure-type/c (Closure/c Lam-type/c BEnv?))


;; -- public


;(: empty-benv BEnv)
(define empty-benv (hash))

(define/ctc-helper ((key-of/c a-hash) k)
  (hash-has-key? a-hash k))

;(: benv-lookup (-> BEnv Var Addr))
(define benv-lookup
  hash-ref)

;(: benv-extend (-> BEnv Var Addr BEnv))
(define benv-extend
  hash-set)

;(: benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
(define (benv-extend* benv vars addrs)
  (for/fold ([benv benv])
    ([v (in-list vars)]
     [a (in-list addrs)])
    (benv-extend benv v a)))

