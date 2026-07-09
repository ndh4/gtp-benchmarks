#lang racket

(require racket/contract
         (for-syntax syntax/parse
                     racket/base)
         racket/class)

(provide (all-defined-out))

(define ((memberof/c l) x)
  (member x l))

(define (count-occurrences l)
  (for/fold ([occurrences (hash)])
            ([elem (in-list l)])
    (hash-update occurrences elem add1 0)))

(define (permutationof/c l)
  (define l-occ (count-occurrences l))
  (λ (x) (equal? l-occ (count-occurrences x))))

(define-syntax (class/c* stx)
  (syntax-parse stx
    #:datum-literals (field/all init-field/all all inherit+super)
    [(_ (~alt (~optional (field/all f-spec ...))
              (~optional (init-field/all i-f-spec ...))
              (~optional (all all-spec ...))
              (~optional (inherit+super i+s-spec ...))) ...
        other-specs ...)
     #'(class/c (~? (init-field i-f-spec ...))
                (~? (field f-spec ...))
                (~? (inherit-field f-spec ... i-f-spec ...))
                ;; all
                (~? (inherit all-spec ...))
                (~? (super all-spec ...))
                (~? (override all-spec ...))
                ;; i+s
                (~? (inherit i+s-spec ...))
                (~? (super i+s-spec ...))
                ;; rest
                other-specs ...)]))

(define (or-#f/c ctc)
  (or/c ctc
        #f))


(define
 (stringof char-pred)
 (flat-named-contract
  `(stringof ,(contract-name char-pred))
  (lambda (s)
    (and (string? s) (for/and ((ch (in-string s))) (char-pred ch))))
  (lambda (fuel)
    (define list-gen (contract-random-generate/choose (listof char-pred) fuel))
    (thunk
      (list->string (list-gen))))))

(define stack? list?)

(define command%/c
  (class/c*
   (init-field/all
    [id symbol?]
    [descr string?]
    [exec ((listof
            (instanceof/c
             (recursive-contract command%/c)))
           stack?
           any/c
           . -> .
           (or/c (cons/c (listof
                             (instanceof/c
                              (recursive-contract command%/c)))
                            stack?)
                  'EXIT
                  #f))])))


(define command%? (instanceof/c command%/c))
(define env? (listof command%?))
(define (command%-with-id/c id-field)
  (and/c command%?
         (lambda (x) (eq? id-field (get-field id x)))))

(define-syntax (command%?-with-exec stx)
  (syntax-parse stx
    #:datum-literals (args type result)
    [(_ (~optional (type command%-c-type))
        (args env-name stack-name val-name)
        [result result-ctc]
        (~optional (~seq (~datum #:post) post-condition)))
     #'(and/c (instanceof/c
               (~? command%-c-type command%/c))
              (instanceof/c
               (class/c*
                (field/all
                 [exec (->i ([env-name env?]
                             [stack-name stack?]
                             [val-name any/c])
                            [result (env-name stack-name val-name)
                                    result-ctc]
                            (~? (~@ #:post
                                    (env-name stack-name val-name)
                                    post-condition)))]))))]))

(define ((list-with-min-size/c n) S)
  (and (list? S)
       (>= (length S) n)))

(define list-with-min-size-two/c
  (cons/c any/c
    (cons/c any/c
      list?)))

(define (not-unequal? v1 v2)
  (match (list v1 v2)
    [(list a b) #:when (eq? a b) #t]
    [(list (? string?) (? string?))
     (string=? v1 v2)]
    [(list (? bytes?) (? bytes?))
     (bytes=? v1 v2)]
    [(list (? number?) (? number?))
     (= v1 v2)]
    [(list (? pair?) (? pair?))
     (and (not-unequal? (car v1)
                         (car v2))
          (not-unequal? (cdr v1)
                         (cdr v2)))]
    [(list (? mpair?) (? mpair?))
     (and (not-unequal? (mcar v1)
                         (mcar v2))
          (not-unequal? (mcdr v1)
                         (mcdr v2)))]
    [(list (? vector?) (? vector?))
     (not-unequal? (vector->list v1)
                    (vector->list v2))]
    [(list (? hash?) (? hash?))
     (define v2-as-list (hash->list v2))
     (and (= (hash-count v1)
             (hash-count v2))
          (andmap (λ (pair1)
                    (ormap (λ (pair2) (not-unequal? pair1 pair2)) v2-as-list)) (hash->list v1)))]
    [(list (? generic-set?) (? generic-set?))
     (define v2-as-list (set->list v2))
     (and (= (set-count v1)
             (set-count v2))
          (andmap (λ (elem1)
                    (ormap (λ (elem2) (not-unequal? elem1 elem2)) v2-as-list)) (set->list v1)))]
    [(list (? object?) (? object?))
     (not-unequal? (object->vector v1) (object->vector v2))]
    [(list (? struct?) (? struct?))
     (not-unequal? (struct->vector v1) (struct->vector v2))]
    [(list (? box?) (? box?))
     (not-unequal? (unbox v1) (unbox v2))]
    [(list (? procedure?) (? procedure?))
     (and (arity=? (procedure-arity v1)
                   (procedure-arity v2))
          ;; FIXME: I can make this stricter.
          ;; Obviously the halting problem is a thing, but perhaps we can random test these two procedures.
          )]
    [else #f]))

(define (equal?/c c/v)
  (flat-named-contract
    (string->symbol (format "(equal?/c ~s)" c/v))
    (lambda (v) (not-unequal? c/v v))))

(define ((thunked-equal?/c c/v) v)
  (equal? (c/v) v))

(struct commutative-binary-predicate-proj ()
  #:property prop:contract
  (build-contract-property
   #:name (λ (c) 'commutative-binary-predicate?)
   #:late-neg-projection (λ (c)
                           (λ (blame)
                             (λ (val neg-party)
                               (cond [(and (procedure? val) (procedure-arity-includes? val 2))
                                      (λ (x y)
                                        (define normal (val x y))
                                        (unless (boolean? normal)
                                          (raise-blame-error blame #:missing-party neg-party val
                                                             "promised predicate, but output was non-boolean: ~e" normal))
                                        (define flipped (val y x))
                                        (if (equal? normal flipped)
                                            normal
                                            (raise-blame-error blame #:missing-party neg-party val
                                                               "promised commutative predicate, but flipping input order changed the outputs: ~e and ~e for inputs ~e and ~e"
                                                               normal flipped x y)))]
                                     [else
                                      (raise-blame-error blame #:missing-party neg-party val
                                                         "promised: a binary predicate~n  produced: ~e" val)]))))
   #:generate
   (lambda (c)
     (lambda (fuel)
       (define cbfs (list eq? equal? eqv? equal-always? (lambda (a b) (and a b #t)) (lambda (a b) (not (or a b)))))
       (thunk (get-random-element cbfs))))))

(define (get-random-element l [rand-gen (current-pseudo-random-generator)])
  (list-ref l (random (length l) rand-gen)))

(define commutative-binary-predicate?
  (commutative-binary-predicate-proj))

(define my-vector?
  (make-contract
   #:name 'my-vector?
   #:late-neg-projection
   (contract-late-neg-projection vector?)
   #:generate
   (λ (fuel)
     (define list-generator
       (contract-random-generate/choose list? fuel))
     (thunk
      (list->vector (list-generator))))))

(define exact-nonnegative-integer?/small-gen
  (flat-named-contract
   'exact-nonnegative-integer?/small-gen
   exact-nonnegative-integer?
   (lambda (fuel)
     (thunk (* fuel (random 0 (random 1 (random 2 (random 3 (random 4 (random 5 (random 6 3000))))))))))))