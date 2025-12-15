#lang racket

;; Abstract Interpretation

(require
  "structs.rkt"
  ;; "benv.rkt"
  (only-in "benv.rkt" Closure Binding BEnv? Closure/c Binding/c Closure-type/c Binding-type/c
           Time? Addr? Closure-lam Closure-benv Binding-var Binding-time)
  ;; "time.rkt"
  (only-in "denotable.rkt" State Denotable/c Store/c State/c State-type? State-call State-benv
           State-store State-time)
  racket/set
  racket/match
  "../../../ctcs/configurable.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
)
(require/configurable-contract "denotable.rkt" store-join store-update* store-update store-lookup empty-store d-join d-bot )
(require/configurable-contract "time.rkt" time-zero take* tick alloc)
(require/configurable-contract "benv.rkt" benv-extend* benv-extend benv-lookup empty-benv )

;; ---

(provide/configurable-contract
 [atom-eval ([max (->i ([benv BEnv?]
                        [store Store/c])
                       [result (benv store)
                               (->i ([id Exp-type/c])
                                    #:pre (id) (match id
                                                 [(Ref _ var)
                                                  (hash-has-key? benv var)]
                                                 [_ #t])
                                    [result Denotable/c]
                                    #:post (id result)
                                    (match id
                                      [(Ref _ var)
                                       (=> fail)
                                       (equal? result (hash-ref store (hash-ref benv var fail) fail))]
                                      [(Ref _ var)
                                       (set-empty? result)]
                                      [(? Lam?)
                                       (equal? result (set (Closure id benv)))]
                                      [_ #f]))])]
             [types (BEnv? Store/c . -> . (Exp-type/c . -> . Denotable/c))])]
 [next ([max ((and/c State-type? closed-State?) . -> . (set/c (and/c State-type?
                                                                     closed-State?)
                                                              #:kind 'immutable))]
        [types (State-type? . -> . (set/c State-type? #:kind 'immutable))])]
 [explore ([max (->i ([seen (set/c (and/c State-type? closed-State?)
                                   #:kind 'immutable)]
                      [todo (listof (and/c State-type? closed-State?))])
                     [result (seen todo)
                             (and/c (set/c (and/c State-type? closed-State?)
                                           #:kind 'immutable)
                                    (subset?/c seen)
                                    (subset?/c (list->set todo)))])]
           [types ((set/c State-type? #:kind 'immutable)
                   (listof State-type?)
                   . -> .
                   (set/c State-type? #:kind 'immutable))])])


(provide
;;   atom-eval
;;   next
;;   explore
  closed-term?
)

;; =============================================================================

;(: atom-eval (-> BEnv Store (-> Exp Denotable)))
(define (atom-eval benv store)
  (λ (id)
    (cond
      [(Ref? id)
       (store-lookup store (benv-lookup benv (Ref-var id)))]
      [(Lam? id)
       (set (Closure id benv))]
      [else
       (error "atom-eval got a plain Exp")])))


(define/ctc-helper (closed-State? st)
  (match-define (State e benv _ _) st)
  (closed-term?/with-env e benv))

(define/ctc-helper (closed-term? e)
  (closed-term?/with-env e empty-benv))

(define/ctc-helper (closed-term?/with-env e benv)
  (match e
    [(Ref _ var) (hash-has-key? benv var)]
    [(Lam _ formals body)
     (closed-term?/with-env body
                            (for/fold ([extended-benv benv])
                                      ([id (in-list formals)])
                              ;; #f: dummy value, just matters that
                              ;; its in there at all
                              (hash-set extended-benv id #f)))]
    [(Call _ f args)
     (andmap (curryr closed-term?/with-env benv)
             (cons f args))]
    [_ #t]))

;(: next (-> State (Setof State)))
(define (next st)
  ;; lltodo: I don't think there's any reason to be more specific
  ;; here. It just calls other functions.
  (match-define (State c benv store time) st)
  (cond
    [(Call? c)
     (define time* (tick c time))
     (match-define (Call _ f args) c)
     ;(: procs Denotable)
     (define procs ((atom-eval benv store) f))
     ;(: params (Listof Denotable))
     (define params (map (atom-eval benv store) args))
     ;(: new-states (Listof State))
     (define new-states
       (for/list ([proc (in-set procs)])
         (match-define (Closure lam benv*) proc)
         (match-define (Lam _ formals call*) lam)
         (define bindings (map (alloc time*) formals))
         (define benv** (benv-extend* benv* formals bindings))
         (define store* (store-update* store bindings params))
         (State call* benv** store* time*)))
     (list->set new-states)]
    [else (set)]))

;; -- state space exploration

(define/ctc-helper ((subset?/c sub) s)
  (subset? sub s))

;(: explore (-> (Setof State) (Listof State) (Setof State)))
(define (explore seen todo)
  (cond
    [(eq? '() todo)
     ;; Nothing left to do
     seen]
    [(set-member? seen (car todo))
     ;; Already seen current todo, move along
     (explore seen (cdr todo))]
    [else
      (define st0 (car todo))
      ;(: succs (Setof State))
      (define succs (next st0))
      (explore (set-add seen st0)
               (append (set->list succs) (cdr todo)))]))

