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
                           [keys benv-extend*-keys-ctc]
                           [vals benv-extend*-vals-ctc])
                          [result BEnv?]
                          #:post (benv keys vals result)
                          (and
                            (for/and ([k (in-list keys)]
                                      [v (in-list vals)]
                                      [i (in-naturals)])
                              (and (hash-has-key? result k)
                                   (or (member k (list-tail keys (add1 i)))
                                       (equal? (hash-ref result k) v))))
                            (clean-benv-extend*-args-env!?)))]
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

(define (lists-eqlen/c* c1 c2)

  (define generator-store (box #f))
  (define predicate-store (box #f))

  (define (reset-predicate-store! . _)
    (set-box! predicate-store #f)
    #t)

  (define (make-list-ctc this-elem/c other-elem/c)
    (define name (format "~a-list?-eqlen-~a-list"
                         (contract-name this-elem/c)
                         (contract-name other-elem/c)))
    (make-contract
     #:name (string->symbol name)
     #:late-neg-projection
     (λ (blame)
       (λ (val neg-party)
         (unless (list? val)
           (raise-blame-error
            blame #:missing-party neg-party
            val
            '(expected "list?" given: "~e") val))
         (let ([len (length val)]
               [stored-len (unbox predicate-store)])
           (match stored-len
             [(? number?)
              (unless (= len stored-len)
                (raise-blame-error
                 blame #:missing-party neg-party
                 val
                 (list 'expected name 'given: "~e") val))]
             [#f
              (set-box! predicate-store len)]))
         (((contract-late-neg-projection (listof this-elem/c)) blame) val neg-party)))
     #:generate
     (λ (fuel)
       (define list-generator (contract-random-generate/choose (listof this-elem/c) fuel))
       (define elem-generator (contract-random-generate/choose this-elem/c (sub1 fuel)))
       (thunk
        (define stored-len (unbox generator-store))
        (match stored-len
          [(? number?)
           (set-box! generator-store #f)
           (for/list ([_ (in-range stored-len)])
             (elem-generator))]
          [#f
           (define result (list-generator))
           (set-box! generator-store (length result))
           result])))))

  (define ctc1 (make-list-ctc c1 c2))
  (define ctc2 (make-list-ctc c2 c1))

  (list ctc1 ctc2 reset-predicate-store!))

(define/ctc-helper benv-extend*-args/c* (lists-eqlen/c* Var? Addr?))
(define/ctc-helper benv-extend*-keys-ctc (first benv-extend*-args/c*))
(define/ctc-helper benv-extend*-vals-ctc (second benv-extend*-args/c*))
(define/ctc-helper clean-benv-extend*-args-env!? (third benv-extend*-args/c*))

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

(module+ test
  (require
    rackunit
    (only-in racket/format ~a))

    (define benv1 (benv-extend empty-benv 'x (Binding 'x '(g123))))
    (check-equal? (benv-lookup benv1 'x) (Binding 'x '(g123)))

    (define benv2 (benv-extend* benv1 '(x k) (list (Binding 'x '(g19)) (Binding 'k '(g19)))))
    (check-equal? (benv-lookup benv2 'k) (Binding 'k '(g19)))
    (check-equal? (benv-lookup benv2 'x) (Binding 'x '(g19)))

)