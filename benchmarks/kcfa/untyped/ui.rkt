#lang racket

(require require-typed-check
         racket/set
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
         (only-in
          "denotable.rkt"
          State
          Denotable/c
          Store/c
          State/c
          State-type?
          State-call
          State-benv
          State-store
          State-time)
         (only-in racket/string string-join)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require (only-in "time.rkt" time-zero take* tick alloc))

(require (only-in
          "denotable.rkt"
          store-join
          store-update*
          store-update
          store-lookup
          empty-store
          d-join
          d-bot))

(require (only-in "benv.rkt" benv-extend* benv-extend benv-lookup empty-benv))

(require (only-in "ai.rkt" explore next atom-eval))

(require (only-in "ai.rkt" closed-term?))

(provide summarize
         empty-mono-store
         monovariant-value
         monovariant-store
         analyze
         format-mono-store)

(define/ctc-helper
 MonoStore/c
 (hash/c Var? (set/c Exp-type/c #:kind 'immutable)))

(define/contract
 (summarize states)
 (configurable-ctc
  (max
   (->i
    ((states (set/c State-type? #:kind 'immutable)))
    (result
     (states)
     (equal?/c (foldl store-join empty-store (set-map states State-store))))))
  (types (-> (set/c State-type? #:kind 'immutable) Store/c)))
 (for/fold
  ((store empty-store))
  ((state (in-set states)))
  (store-join (State-store state) store)))

(define/contract
 empty-mono-store
 (configurable-ctc (max MonoStore/c) (types MonoStore/c))
 (hash))

(define/contract
 (monovariant-value v)
 (configurable-ctc
  (max (->i ((v Closure-type/c)) (result (v) (equal?/c (Closure-lam v)))))
  (types (-> Closure-type/c Lam-type/c)))
 (Closure-lam v))

(define/contract
 (monovariant-store store)
 (configurable-ctc
  (max
   (->i
    ((store Store/c))
    (result MonoStore/c)
    #:post
    (store result)
    (for/and
     (((b vs) (in-hash store)))
     (define result-vs (hash-ref result (Binding-var b) set))
     (define mono-vs (list->set (set-map vs monovariant-value)))
     (subset? mono-vs result-vs))))
  (types (-> Store/c MonoStore/c)))
 (define (update-lam vs)
   (λ (b-vs)
     (define v-vs (list->set (set-map vs monovariant-value)))
     (set-union b-vs v-vs)))
 (define (default-lam) (set))
 (for/fold
  ((mono-store empty-mono-store))
  (((b vs) (in-hash store)))
  (hash-update mono-store (Binding-var b) (update-lam vs) default-lam)))

(define/contract
 (analyze exp)
 (configurable-ctc
  (max (->i ((exp (and/c Exp-type/c closed-term?))) (result MonoStore/c)))
  (types (-> Exp-type/c MonoStore/c)))
 (define init-state (State exp empty-benv empty-store time-zero))
 (define states (explore (set) (list init-state)))
 (define summary (summarize states))
 (define mono-store (monovariant-store summary))
 mono-store)

(define/contract
 (format-mono-store ms)
 (configurable-ctc
  (max (-> MonoStore/c string?))
  (types (-> MonoStore/c string?)))
 (define res
   (for/list
    (((i vs) (in-hash ms)))
    (format
     "~a:\n~a"
     i
     (string-join (for/list ((v (in-set vs))) (format "\t~S" v)) "\n"))))
 (string-join res "\n"))

#;(module+
 test
 (require rackunit (only-in racket/format ~a))
 (define (sets-equal? s1 s2)
   (and (all-in? (set->list s1) (set->list s2))
        (all-in? (set->list s2) (set->list s1))))
 (define (all-in? l1 l2) (not (not (for/and ((elem l1)) (member elem l2)))))
 (check-equal? (format-mono-store empty-mono-store) "")
 (define init-state__smallest
   (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '()))
 (define states__smallest (explore (set) (list init-state__smallest)))
 (check-equal?
  states__smallest
  (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
 (define summary__smallest (summarize states__smallest))
 (check-equal? summary__smallest '#hash())
 (define mono-store__smallest (monovariant-store summary__smallest))
 (check-equal? mono-store__smallest '#hash())
 (define init-state__tiny
   (State
    (Call 'g6 (Lam 'g3 '(a) (Ref 'g2 'a)) (list (Lam 'g5 '(b) (Ref 'g4 'b))))
    '#hash()
    '#hash()
    '()))
 (define states__tiny (explore (set) (list init-state__tiny)))
 (check-true
  (sets-equal?
   states__tiny
   (set
    (State
     (Call 'g6 (Lam 'g3 '(a) (Ref 'g2 'a)) (list (Lam 'g5 '(b) (Ref 'g4 'b))))
     '#hash()
     '#hash()
     '())
    (State
     (Ref 'g2 'a)
     (hash 'a (Binding 'a '(g6)))
     (hash
      (Binding 'a '(g6))
      (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())))
     '(g6)))))
 (define summary__tiny (summarize states__tiny))
 (check-equal?
  summary__tiny
  (hash
   (Binding 'a '(g6))
   (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash()))))
 (define mono-store__tiny (monovariant-store summary__tiny))
 (check-equal? mono-store__tiny (hash 'a (set (Lam 'g5 '(b) (Ref 'g4 'b)))))
 (define init-state__standard
   (State
    (Call
     'g23
     (Lam
      'g22
      '(id)
      (Call
       'g21
       (Ref 'g11 'id)
       (list
        (Lam 'g13 '(z) (Ref 'g12 'z))
        (Lam
         'g20
         '(a)
         (Call
          'g19
          (Ref 'g14 'id)
          (list
           (Lam 'g16 '(y) (Ref 'g15 'y))
           (Lam 'g18 '(b) (Ref 'g17 'b))))))))
     (list (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))))
    '#hash()
    '#hash()
    '()))
 (define states__standard (explore (set) (list init-state__standard)))
 (check-true
  (sets-equal?
   states__standard
   (set
    (State
     (Call
      'g21
      (Ref 'g11 'id)
      (list
       (Lam 'g13 '(z) (Ref 'g12 'z))
       (Lam
        'g20
        '(a)
        (Call
         'g19
         (Ref 'g14 'id)
         (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))))
     (hash 'id (Binding 'id '(g23)))
     (hash
      (Binding 'id '(g23))
      (set
       (Closure
        (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
        '#hash())))
     '(g23))
    (State
     (Ref 'g17 'b)
     (hash
      'a
      (Binding 'a '(g9))
      'b
      (Binding 'b '(g9))
      'id
      (Binding 'id '(g23)))
     (hash
      (Binding 'a '(g9))
      (set
       (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))
      (Binding 'b '(g9))
      (set
       (Closure
        (Lam 'g16 '(y) (Ref 'g15 'y))
        (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
      (Binding 'id '(g23))
      (set
       (Closure
        (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
        '#hash()))
      (Binding 'k '(g21))
      (set
       (Closure
        (Lam
         'g20
         '(a)
         (Call
          'g19
          (Ref 'g14 'id)
          (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
        (hash 'id (Binding 'id '(g23)))))
      (Binding 'x '(g19))
      (set
       (Closure
        (Lam 'g16 '(y) (Ref 'g15 'y))
        (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
      (Binding 'x '(g21))
      (set
       (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))
      (Binding 'k '(g19))
      (set
       (Closure
        (Lam 'g18 '(b) (Ref 'g17 'b))
        (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23))))))
     '(g9))
    (State
     (Call
      'g19
      (Ref 'g14 'id)
      (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b))))
     (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))
     (hash
      (Binding 'a '(g9))
      (set
       (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))
      (Binding 'id '(g23))
      (set
       (Closure
        (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
        '#hash()))
      (Binding 'k '(g21))
      (set
       (Closure
        (Lam
         'g20
         '(a)
         (Call
          'g19
          (Ref 'g14 'id)
          (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
        (hash 'id (Binding 'id '(g23)))))
      (Binding 'x '(g21))
      (set
       (Closure
        (Lam 'g13 '(z) (Ref 'g12 'z))
        (hash 'id (Binding 'id '(g23))))))
     '(g9))
    (State
     (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x)))
     (hash 'k (Binding 'k '(g19)) 'x (Binding 'x '(g19)))
     (hash
      (Binding 'a '(g9))
      (set
       (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))
      (Binding 'id '(g23))
      (set
       (Closure
        (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
        '#hash()))
      (Binding 'k '(g21))
      (set
       (Closure
        (Lam
         'g20
         '(a)
         (Call
          'g19
          (Ref 'g14 'id)
          (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
        (hash 'id (Binding 'id '(g23)))))
      (Binding 'x '(g19))
      (set
       (Closure
        (Lam 'g16 '(y) (Ref 'g15 'y))
        (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
      (Binding 'x '(g21))
      (set
       (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))
      (Binding 'k '(g19))
      (set
       (Closure
        (Lam 'g18 '(b) (Ref 'g17 'b))
        (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23))))))
     '(g19))
    (State
     (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x)))
     (hash 'k (Binding 'k '(g21)) 'x (Binding 'x '(g21)))
     (hash
      (Binding 'id '(g23))
      (set
       (Closure
        (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
        '#hash()))
      (Binding 'k '(g21))
      (set
       (Closure
        (Lam
         'g20
         '(a)
         (Call
          'g19
          (Ref 'g14 'id)
          (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
        (hash 'id (Binding 'id '(g23)))))
      (Binding 'x '(g21))
      (set
       (Closure
        (Lam 'g13 '(z) (Ref 'g12 'z))
        (hash 'id (Binding 'id '(g23))))))
     '(g21))
    (State
     (Call
      'g23
      (Lam
       'g22
       '(id)
       (Call
        'g21
        (Ref 'g11 'id)
        (list
         (Lam 'g13 '(z) (Ref 'g12 'z))
         (Lam
          'g20
          '(a)
          (Call
           'g19
           (Ref 'g14 'id)
           (list
            (Lam 'g16 '(y) (Ref 'g15 'y))
            (Lam 'g18 '(b) (Ref 'g17 'b))))))))
      (list (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))))
     '#hash()
     '#hash()
     '()))))
 (define summary__standard (summarize states__standard))
 (check-equal?
  summary__standard
  (hash
   (Binding 'a '(g9))
   (set
    (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))
   (Binding 'b '(g9))
   (set
    (Closure
     (Lam 'g16 '(y) (Ref 'g15 'y))
     (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
   (Binding 'id '(g23))
   (set
    (Closure
     (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
     '#hash()))
   (Binding 'k '(g21))
   (set
    (Closure
     (Lam
      'g20
      '(a)
      (Call
       'g19
       (Ref 'g14 'id)
       (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
     (hash 'id (Binding 'id '(g23)))))
   (Binding 'x '(g19))
   (set
    (Closure
     (Lam 'g16 '(y) (Ref 'g15 'y))
     (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
   (Binding 'x '(g21))
   (set
    (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23)))))
   (Binding 'k '(g19))
   (set
    (Closure
     (Lam 'g18 '(b) (Ref 'g17 'b))
     (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))))
 (define mono-store__standard (monovariant-store summary__standard))
 (check-equal?
  mono-store__standard
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

