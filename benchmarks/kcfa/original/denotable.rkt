#lang racket

(require racket/set
         racket/list
         "structs.rkt"
         (only-in
          "benv.rkt"
          Closure
          Binding
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
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt")

(require (only-in "time.rkt" time-zero take* tick alloc))

(require (only-in "benv.rkt" benv-extend* benv-extend benv-lookup empty-benv))

(provide d-bot
         d-join
         empty-store
         store-lookup
         store-update
         store-update*
         store-join)

(provide (struct-out State)
         Denotable/c
         Store/c
         State/c
         State-type?
         State-call
         State-benv
         State-store
         State-time)

(define/ctc-helper Denotable/c (set/c Closure-type/c #:kind 'immutable))

(define/ctc-helper Store/c (hash/c Addr? Denotable/c #:immutable #t))

(struct State (call benv store time) #:mutable #:transparent)

(define/ctc-helper
 (State/c call/c benv/c store/c time/c)
 (struct/c State call/c benv/c store/c time/c))

(define/ctc-helper State-type? (State/c Stx-type/c BEnv? Store/c Time?))

(define/contract
 d-bot
 (configurable-ctc (max Denotable/c) (types Denotable/c))
 (set))

(define/contract
 d-join
 (configurable-ctc
  (max
   (->i
    ((a Denotable/c) (b Denotable/c))
    (result Denotable/c)
    #:post
    (a b result)
    (for/and
     ((el (in-sequences (in-set a) (in-set b))))
     (set-member? result el))))
  (types (-> Denotable/c Denotable/c Denotable/c)))
 set-union)

(define/contract
 empty-store
 (configurable-ctc (max Store/c) (types Store/c))
 (hash))

(define/contract
 (store-lookup s a)
 (configurable-ctc
  (max
   (->i
    ((s Store/c) (a Addr?))
    (result (s a) (equal?/c (if (hash-has-key? s a) (hash-ref s a) d-bot)))))
  (types (-> Store/c Addr? Denotable/c)))
 (hash-ref s a (lambda () d-bot)))

(define/ctc-helper
 ((hash-with/c key val-ok?) h)
 (and (hash? h) (hash-has-key? h key) (val-ok? (hash-ref h key))))

(define/contract
 (store-update store addr value)
 (configurable-ctc
  (max
   (->i
    ((s Store/c) (addr Addr?) (value Denotable/c))
    (result
     (s addr value)
     (and/c
      Store/c
      (hash-with/c addr (equal?/c (set-union value (hash-ref s addr set))))))))
  (types (-> Store/c Addr? Denotable/c Store/c)))
 (define (update-lam d) (d-join d value))
 (hash-update store addr update-lam (lambda () d-bot)))

(define/contract
 (store-update* s as vs)
 (configurable-ctc
  (max
   (->i
    ((s Store/c) (as (listof Addr?)) (vs (listof Denotable/c)))
    (result Store/c)
    #:post
    (s as vs result)
    (for/and
     ((a (in-list as)) (v (in-list vs)))
     (and (hash-has-key? result a) (subset? v (hash-ref result a))))))
  (types (-> Store/c (listof Addr?) (listof Denotable/c) Store/c)))
 (for/fold
  ((store s))
  ((a (in-list as)) (v (in-list vs)))
  (store-update store a v)))

(define/contract
 (store-join s1 s2)
 (configurable-ctc
  (max
   (->i
    ((s1 Store/c) (s2 Store/c))
    (result Store/c)
    #:post
    (s1 s2 result)
    (for/and
     (((k v) (in-hash result)))
     (equal? v (set-union (hash-ref s1 k set) (hash-ref s2 k set))))))
  (types (-> Store/c Store/c Store/c)))
 (for/fold
  ((new-store s1))
  (((k v) (in-hash s2)))
  (store-update new-store k v)))

(module+
 test
 (require rackunit (only-in racket/format ~a))
 (check-equal? (store-join empty-store empty-store) empty-store)
 (define s1
   (store-update
    empty-store
    (Binding 'a '(g6))
    (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash()))))
 (check-equal?
  (store-lookup s1 (Binding 'a '(g6)))
  (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())))
 (define s2
   (store-update*
    empty-store
    (list (Binding 'x '(g21)) (Binding 'k '(g21)))
    (list
     (set
      (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))
     (set
      (Closure
       (Lam
        'g20
        '(a)
        (Call
         'g19
         (Ref 'g14 'id)
         (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
       (hash 'id (Binding 'id '(g23))))))))
 (check-equal?
  (store-lookup s2 (Binding 'x '(g21)))
  (set
   (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23))))))
 (check-equal?
  (store-lookup s2 (Binding 'k '(g21)))
  (set
   (Closure
    (Lam
     'g20
     '(a)
     (Call
      'g19
      (Ref 'g14 'id)
      (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
    (hash 'id (Binding 'id '(g23))))))
 (define s3 (store-join s1 s2))
 (check-equal?
  (store-lookup s3 (Binding 'a '(g6)))
  (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())))
 (check-equal?
  (store-lookup s3 (Binding 'x '(g21)))
  (set
   (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23))))))
 (check-equal?
  (store-lookup s3 (Binding 'k '(g21)))
  (set
   (Closure
    (Lam
     'g20
     '(a)
     (Call
      'g19
      (Ref 'g14 'id)
      (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
    (hash 'id (Binding 'id '(g23))))))
 (define s4
   (store-update
    s3
    (Binding 'a '(g6))
    (set
     (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))))
 (check-equal?
  (store-lookup s4 (Binding 'a '(g6)))
  (set
   (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())
   (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))))

