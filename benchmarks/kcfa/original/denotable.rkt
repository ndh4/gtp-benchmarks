#lang racket
;; Denotable values and stores to hold them.
;; A denotable is a set of values
;; (A value is a closure)

(require
  require-typed-check
  racket/set
  racket/list
  "structs.rkt"
  (only-in "benv.rkt" Closure Binding BEnv? Closure/c Binding/c Closure-type/c Binding-type/c
           Time? Addr? Closure-lam Closure-benv Binding-var Binding-time)
  ;; "time.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  "../../../ctcs/configurable.rkt"
  )
(require/configurable-contract "time.rkt" time-zero take* tick alloc)
(require/configurable-contract "benv.rkt" benv-extend* benv-extend benv-lookup empty-benv )

;; -----------------------------------------------------------------------------
(provide/configurable-contract
 [d-bot ([max Denotable/c]
         [types Denotable/c])]
 [d-join ([max (->i ([a Denotable/c]
              [b Denotable/c])
             [result Denotable/c]
             #:post (a b result)
             (for/and ([el (in-sequences (in-set a) (in-set b))])
               (set-member? result el)))]
   [types (Denotable/c Denotable/c . -> . Denotable/c)])]
 [empty-store ([max Store/c]
               [types Store/c])]
 [store-lookup ([max (->i ([s Store/c]
              [a Addr?])
             [result (s a)
                     (equal?/c (if (hash-has-key? s a)
                                   (hash-ref s a)
                                   d-bot))])]
   [types (Store/c Addr? . -> . Denotable/c)])]
 [store-update ([max (->i ([s Store/c]
              [addr Addr?]
              [value Denotable/c])
             [result (s addr value)
                     (and/c
                      Store/c
                      (hash-with/c addr
                                   (equal?/c
                                    (set-union value
                                               (hash-ref s addr set)))))])]
   [types (Store/c Addr? Denotable/c . -> . Store/c)])]
 [store-update* ([max (->i ([s Store/c]
              [as (listof Addr?)]
              [vs (listof Denotable/c)])
             [result Store/c]
             #:post (s as vs result)
             (for/and ([a (in-list as)]
                       [v (in-list vs)])
               (and (hash-has-key? result a)
                    (subset? v (hash-ref result a)))))]
   [types (Store/c (listof Addr?) (listof Denotable/c) . -> . Store/c)])]
 [store-join ([max (->i ([s1 Store/c]
              [s2 Store/c])
             [result Store/c]
             #:post (s1 s2 result)
             (for/and ([(k v) (in-hash result)])
               (equal? v (set-union (hash-ref s1 k set)
                                    (hash-ref s2 k set)))))]
   [types (Store/c Store/c . -> . Store/c)])])
(provide
  (struct-out State)
;;   d-bot
;;   d-join
;;   empty-store
;;   store-lookup
;;   store-update
;;   store-update*
;;   store-join

  Denotable/c
  Store/c
  State/c
  State-type?
  State-call
  State-benv
  State-store
  State-time
)

;; =============================================================================

;; -- private
;(define-type Denotable (Setof Value))
;(define-type Store (HashTable Addr Denotable))

(define/ctc-helper Denotable/c (set/c Closure-type/c #:kind 'immutable))
(define/ctc-helper Store/c (hash/c Addr? Denotable/c #:immutable #t))

;; -- structs

(struct State
 (call ;: Exp]
  benv ;: BEnv]
  store ;: Store]
  time ;: Time]))
  )
  #:mutable
  #:transparent)

(define/ctc-helper (State/c call/c benv/c store/c time/c)
  (struct/c State call/c benv/c store/c time/c))
(define/ctc-helper State-type? (State/c Stx-type/c BEnv? Store/c Time?))

;; -- public

;(: d-bot Denotable)
(define d-bot
  (set))

;(: d-join (-> Denotable Denotable Denotable))
(define d-join
  set-union)

;(: empty-store Store)
(define empty-store
  (hash))

;(: store-lookup (-> Store Addr Denotable))
(define (store-lookup s a)
  (hash-ref s a (lambda () d-bot)))

;; any/c flat-contract? (hash/c any/c any/c) -> flat-contract?
(define/ctc-helper ((hash-with/c key val-ok?) h)
  (and (hash? h)
       (hash-has-key? h key)
       (val-ok? (hash-ref h key))))

;(: store-update (-> Store Addr Denotable Store))
(define (store-update store addr value)
  ;(: update-lam (-> Denotable Denotable))
  (define (update-lam d) (d-join d value))
  (hash-update store addr update-lam (lambda () d-bot)))

;(: store-update* (-> Store (Listof Addr) (Listof Denotable) Store))
(define (store-update* s as vs)
  (for/fold ([store s])
    ([a (in-list as)]
     [v (in-list vs)])
    (store-update store a v)))

;(: store-join (-> Store Store Store))
(define (store-join s1 s2)
  (for/fold ([new-store s1])
    ([(k v) (in-hash s2)])
    (store-update new-store k v)))
