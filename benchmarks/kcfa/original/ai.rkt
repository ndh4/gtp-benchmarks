#lang racket

(require "structs.rkt"
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
         racket/set
         racket/match
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require (only-in
          "denotable.rkt"
          store-join
          store-update*
          store-update
          store-lookup
          empty-store
          d-join
          d-bot))

(require (only-in "time.rkt" time-zero take* tick alloc))

(require (only-in "benv.rkt" benv-extend* benv-extend benv-lookup empty-benv))

(provide atom-eval next explore)

(provide closed-term?)

(define/contract
 (atom-eval benv store)
 (configurable-ctc
  (max
   (->i
    ((benv BEnv?) (store Store/c))
    (result
     (benv store)
     (->i
      ((id Exp-type/c))
      #:pre
      (id)
      (match id ((Ref _ var) (hash-has-key? benv var)) (_ #t))
      (result Denotable/c)
      #:post
      (id result)
      (match
       id
       ((Ref _ var)
        (=> fail)
        (equal? result (hash-ref store (hash-ref benv var fail) fail)))
       ((Ref _ var) (set-empty? result))
       ((? Lam?) (equal? result (set (Closure id benv))))
       (_ #f))))))
  (types (-> BEnv? Store/c (-> Exp-type/c Denotable/c))))
 (λ (id)
   (cond
    ((Ref? id) (store-lookup store (benv-lookup benv (Ref-var id))))
    ((Lam? id) (set (Closure id benv)))
    (else (error "atom-eval got a plain Exp")))))

(define/ctc-helper
 (closed-State? st)
 (match-define (State e benv _ _) st)
 (closed-term?/with-env e benv))

(define/ctc-helper (closed-term? e) (closed-term?/with-env e empty-benv))

(define/ctc-helper
 (closed-term?/with-env e benv)
 (match
  e
  ((Ref _ var) (hash-has-key? benv var))
  ((Lam _ formals body)
   (closed-term?/with-env
    body
    (for/fold
     ((extended-benv benv))
     ((id (in-list formals)))
     (hash-set extended-benv id #f))))
  ((Call _ f args) (andmap (curryr closed-term?/with-env benv) (cons f args)))
  (_ #t)))

(define/contract
 (next st)
 (configurable-ctc
  (max
   (->
    (and/c State-type? closed-State?)
    (set/c (and/c State-type? closed-State?) #:kind 'immutable)))
  (types (-> State-type? (set/c State-type? #:kind 'immutable))))
 (match-define (State c benv store time) st)
 (cond
  ((Call? c)
   (define time* (tick c time))
   (match-define (Call _ f args) c)
   (define procs ((atom-eval benv store) f))
   (define params (map (atom-eval benv store) args))
   (define new-states
     (for/list
      ((proc (in-set procs)))
      (match-define (Closure lam benv*) proc)
      (match-define (Lam _ formals call*) lam)
      (define bindings (map (alloc time*) formals))
      (define benv** (benv-extend* benv* formals bindings))
      (define store* (store-update* store bindings params))
      (State call* benv** store* time*)))
   (list->set new-states))
  (else (set))))

(define/ctc-helper ((subset?/c sub) s) (subset? sub s))

(define/contract
 (explore seen todo)
 (configurable-ctc
  (max
   (->i
    ((seen (set/c (and/c State-type? closed-State?) #:kind 'immutable))
     (todo (listof (and/c State-type? closed-State?))))
    (result
     (seen todo)
     (and/c
      (set/c (and/c State-type? closed-State?) #:kind 'immutable)
      (subset?/c seen)
      (subset?/c (list->set todo))))))
  (types
   (->
    (set/c State-type? #:kind 'immutable)
    (listof State-type?)
    (set/c State-type? #:kind 'immutable))))
 (cond
  ((eq? '() todo) seen)
  ((set-member? seen (car todo)) (explore seen (cdr todo)))
  (else
   (define st0 (car todo))
   (define succs (next st0))
   (explore (set-add seen st0) (append (set->list succs) (cdr todo))))))

(module+
 test
 (require rackunit (only-in racket/format ~a))
 (define (sets-equal? s1 s2)
   (and (all-in? (set->list s1) (set->list s2))
        (all-in? (set->list s2) (set->list s1))))
 (define (all-in? l1 l2) (not (not (for/and ((elem l1)) (member elem l2)))))
 (check-equal?
  ((atom-eval
    (hash 'id (Binding 'id '(g23)))
    (hash
     (Binding 'id '(g23))
     (set
      (Closure
       (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
       '#hash()))))
   (Lam 'g13 '(z) (Ref 'g12 'z)))
  (set
   (Closure (Lam 'g13 '(z) (Ref 'g12 'z)) (hash 'id (Binding 'id '(g23))))))
 (check-exn
  #rx"atom-eval got a plain Exp"
  (λ ()
    ((atom-eval
      (hash 'id (Binding 'id '(g23)))
      (hash
       (Binding 'id '(g23))
       (set
        (Closure
         (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
         '#hash()))))
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
          (Lam 'g18 '(b) (Ref 'g17 'b))))))))))
 (check-equal?
  ((atom-eval
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
       (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))))
   (Ref 'g7 'k))
  (set
   (Closure
    (Lam 'g18 '(b) (Ref 'g17 'b))
    (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23))))))
 (check-equal?
  (next (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '()))
  (set))
 (check-true
  (sets-equal?
   (next
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
     '(g19)))
   (set
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
     '(g9)))))
 (check-equal?
  (explore (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())) '())
  (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
 (check-equal?
  (explore
   (set)
   (list (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
  (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
 (check-equal?
  (explore
   (set)
   (list
    (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())
    (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
  (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
 (check-true
  (sets-equal?
   (explore
    (set)
    (list
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
      '())))
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
     '())))))

