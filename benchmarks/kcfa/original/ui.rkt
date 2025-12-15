#lang racket

;; User Interface to `ai.rkt`

(require
  require-typed-check
  racket/set
  "structs.rkt"
  ;; "benv.rkt"
  (only-in "benv.rkt" Closure Binding BEnv? Closure/c Binding/c Closure-type/c Binding-type/c
           Time? Addr? Closure-lam Closure-benv Binding-var Binding-time)
  ;; "denotable.rkt"
    (only-in "denotable.rkt" State Denotable/c Store/c State/c State-type? State-call State-benv State-store State-time)

  ;; "time.rkt"
  (only-in racket/string string-join)
  "../../../ctcs/configurable.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
)
(require/configurable-contract "time.rkt" time-zero take* tick alloc)
(require/configurable-contract "denotable.rkt" store-join store-update* store-update store-lookup empty-store d-join d-bot )
(require/configurable-contract "benv.rkt" benv-extend* benv-extend benv-lookup empty-benv )

(require/configurable-contract "ai.rkt" explore next atom-eval )
(require (only-in "ai.rkt" closed-term?))
;(require/typed/check "ai.rkt"
;  (atom-eval (-> BEnv Store (-> Exp Denotable)))
;  (next (-> State (Setof State)))
;  (explore (-> (Setof State) (Listof State) (Setof State)))
;)
;; ---

(provide/configurable-contract
 [summarize ([max (->i ([states (set/c State-type? #:kind 'immutable)])
                       [result (states)
                               (equal?/c
                                (foldl store-join
                                       empty-store
                                       (set-map states State-store)))])]
             [types ((set/c State-type? #:kind 'immutable) . -> . Store/c)])]
 [empty-mono-store ([max MonoStore/c]
                    [types MonoStore/c])]
 [monovariant-value ([max (->i ([v Closure-type/c])
                               [result (v) (equal?/c (Closure-lam v))])]
                     [types (Closure-type/c . -> . Lam-type/c)])]
 [monovariant-store ([max (->i ([store Store/c])
                               [result MonoStore/c]
                               #:post (store result)
                               (for/and ([(b vs) (in-hash store)])
                                 (define result-vs (hash-ref result (Binding-var b) set))
                                 (define mono-vs (list->set (set-map vs monovariant-value)))
                                 (subset? mono-vs result-vs)))]
                     [types (Store/c . -> . MonoStore/c)])]
 [analyze ([max (->i ([exp (and/c Exp-type/c closed-term?)])
                     ;; lltodo: can be stronger?
                     [result MonoStore/c])]
           [types (Exp-type/c . -> . MonoStore/c)])]
 [format-mono-store ([max (MonoStore/c . -> . string?)]
                     [types (MonoStore/c . -> . string?)])])

;; (provide
;;   summarize
;;   empty-mono-store
;;   monovariant-value
;;   monovariant-store
;;   analyze
;;   format-mono-store
;; )

;; =============================================================================


;; -- ui.rkt
;(define-type MonoStore (HashTable Var (Setof Exp)))
(define/ctc-helper MonoStore/c (hash/c Var?
                                       (set/c Exp-type/c #:kind 'immutable)))

;(: summarize (-> (Setof State) Store))
(define (summarize states)
  (for/fold ([store empty-store])
    ([state (in-set states)])
    (store-join (State-store state) store)))

;(: empty-mono-store MonoStore)
(define empty-mono-store
  (hash))

;(: monovariant-value (-> Value Lam))
(define (monovariant-value v)
  (Closure-lam v))

;(: monovariant-store (-> Store MonoStore))
(define (monovariant-store store)
  ;(: update-lam (-> (Setof Value) (-> (Setof Exp) (Setof Exp))))
  (define (update-lam vs)
    ;(: v-vs (Setof Lam))
    (λ (b-vs)
      (define v-vs (list->set (set-map vs monovariant-value)))
      (set-union b-vs v-vs)))
  ;(: default-lam (-> (Setof Exp)))
  (define (default-lam) (set))
  (for/fold ([mono-store empty-mono-store])
    ([(b vs) (in-hash store)])
    (hash-update mono-store
                 (Binding-var b)
                 (update-lam vs)
                 default-lam)))

;(: analyze (-> Exp MonoStore))
(define (analyze exp)
  (define init-state (State exp empty-benv empty-store time-zero))
  (define states (explore (set) (list init-state)))
  (define summary (summarize states))
  (define mono-store (monovariant-store summary))
  mono-store)

;(: format-mono-store (-> MonoStore String))
(define (format-mono-store ms)
  ;(: res (Listof String))
  (define res
    (for/list ([(i vs) (in-hash ms)])
      (format "~a:\n~a"
              i
              (string-join
                (for/list ([v (in-set vs)])
                  (format "\t~S" v))
                "\n"))))
  (string-join res "\n"))

