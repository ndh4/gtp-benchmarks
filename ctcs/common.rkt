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
 (stringof char-pred #:non-empty? [non-empty? #f])
 (flat-named-contract
  `(stringof ,(contract-name char-pred) ,@(if non-empty? '(#:non-empty? #t) '()))
  (lambda (s)
    (and ((if non-empty? non-empty-string? string?) s) (for/and ((ch (in-string s))) (char-pred ch))))
  (lambda (fuel)
    (define list-gen (contract-random-generate/choose ((if non-empty? non-empty-listof listof) char-pred) fuel))
    (thunk
      (list->string (list-gen))))))

(define stack? list?)

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

(struct my-vector-contract (args)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (λ (c)
     (string->symbol
      (format "(my-vector/c ~a)"
              (string-join (map ~a (my-vector-contract-args c)) " "))))

   #:late-neg-projection
   (λ (c)
     (contract-late-neg-projection (apply vector/c (my-vector-contract-args c))))

   #:generate
   (λ (c)
     (λ (fuel)
       (define args (my-vector-contract-args c))
       (define list-generator
         (contract-random-generate/choose (apply list/c args) fuel))
       (thunk (list->vector (list-generator)))))))

(define (my-vector/c . args)
  (my-vector-contract args))

(define (my-vectorof arg)
  (make-contract
   #:name (string->symbol (format "(my-vectorof ~a)" arg))
   #:late-neg-projection
   (contract-late-neg-projection (vectorof arg))
   #:generate
   (λ (fuel)
     (define list-generator
       (contract-random-generate/choose (listof arg) fuel))
     (thunk
      (list->vector (list-generator))))))

(define (generate-small-eni lowness high)
  (if (negative? lowness)
      high
      (generate-small-eni (sub1 lowness) (random lowness high))))

(define exact-nonnegative-integer?/small-gen
  (flat-named-contract
   'exact-nonnegative-integer?/small-gen
   exact-nonnegative-integer?
   (lambda (fuel) (thunk (* fuel (generate-small-eni 6 3000))))))

(define exact-nonnegative-integer?/tiny-gen
  (flat-named-contract
   'exact-nonnegative-integer?/tiny-gen
   exact-nonnegative-integer?
   (lambda (fuel) (thunk (random 4)))))

(define (class-ctc->instance-ctc class-ctc)
  (make-contract
   #:name `(class-ctc->instance-ctc ,(contract-name class-ctc))
   #:first-order (λ (v)
                   (and (object? v)
                        (let-values ([(cls skipped?) (object-info v)])
                          (and cls (contract-first-order-passes? class-ctc cls)))))
   #:generate (λ (fuel)
                (define gen-class-thunk (contract-random-generate/choose class-ctc fuel))
                (λ ()
                  (define generated-class (gen-class-thunk))
                  (new generated-class)))))

(define (action/c action)
  (flat-named-contract
   'action/c
   (lambda (x) (action) #t)))

(define (make-dependent-contract ctc1 ctc2 [custom-second-gen #f] #:none-val [none-val #f])
  (define base-ctc (coerce-contract 'make-dependent-contract ctc1))

  (define predicate-store (box none-val))
  (define generator-store (box none-val))

  (define (reset-predicate-store!? . _)
    (set-box! predicate-store none-val)
    #t)

  (define dependent-first-ctc
    (make-contract
     #:name (contract-name base-ctc)
     #:late-neg-projection
     (λ (blame)
       (define base-proj ((contract-late-neg-projection base-ctc) blame))
       (λ (val neg-party)
         (define checked-val (base-proj val neg-party))
         (set-box! predicate-store checked-val)
         checked-val))
     #:generate
     (λ (fuel)
       (define base-gen (contract-random-generate/choose base-ctc fuel))
       (λ ()
         (define result (base-gen))
         (set-box! generator-store result)
         result))))

  (define dependent-second-ctc
    (make-contract
     #:name 'dependent-second?
     #:late-neg-projection
     (λ (blame)
       (λ (val neg-party)
         (match (unbox predicate-store)
           [(== none-val) (raise-blame-error blame #:missing-party neg-party val
                                  "dependent contract lifecycle out of order")]
           [stored-val
            (define dynamic-ctc2 (coerce-contract 'make-dependent-contract (ctc2 stored-val)))
            (define ctc2-proj ((contract-late-neg-projection dynamic-ctc2) blame))
            (ctc2-proj val neg-party)])))
     #:generate
     (λ (fuel)
       (λ ()
         (match (unbox generator-store)
           [(== none-val) (error 'make-dependent-contract "generator lifecycle out of order")]
           [stored-val
            ;; Clear the store state immediately
            (set-box! generator-store none-val)
            (cond
              ;; If a custom sampling macro/function is provided, bypass blind generation
              [custom-second-gen
               (custom-second-gen stored-val fuel)]
              [else
               (define dynamic-ctc2 (coerce-contract 'make-dependent-contract (ctc2 stored-val)))
               (contract-random-generate dynamic-ctc2 fuel)])])))))

  (list dependent-first-ctc dependent-second-ctc reset-predicate-store!?))

(define (contract-with-generator base-contract custom-gen-proc)
  (let ([ctc (coerce-contract 'contract-with-generator base-contract)])
    (make-contract
     #:name (contract-name ctc)
     #:first-order (contract-first-order ctc)
     #:late-neg-projection (contract-late-neg-projection ctc)
     #:generate (lambda (fuel)
                  (lambda ()
                    (custom-gen-proc fuel))))))

(struct array-of-contract (val-ctc)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (lambda (c) `(arrayof ,(contract-name (array-of-contract-val-ctc c))))

   #:first-order
   (lambda (c)
     (lambda (x)
       (and (vector? x)
            (> (vector-length x) 0)
            (let ([first-row (vector-ref x 0)])
              (and (vector? first-row)
                   (for/and ([row (in-vector x)])
                     (and (vector? row)
                          (= (vector-length row) (vector-length first-row))
                          (for/and ([val (in-vector row)])
                            (contract-first-order-passes? (array-of-contract-val-ctc c) val)))))))))

   #:late-neg-projection
   (lambda (c)
     (lambda (blame)
       (lambda (x missing-party)
         (unless (and (vector? x) (> (vector-length x) 0))
           (raise-blame-error blame #:missing-party missing-party x "expected a non-empty vector of vectors"))
         (define first-row (vector-ref x 0))
         (unless (vector? first-row)
           (raise-blame-error blame #:missing-party missing-party x "expected rows to be vectors"))
         (define expected-len (vector-length first-row))

         (define elem-proj ((contract-late-neg-projection (array-of-contract-val-ctc c)) blame))

         (chaperone-vector
          x
          (lambda (vec idx row)
            (unless (and (vector? row) (= (vector-length row) expected-len))
              (raise-blame-error blame #:missing-party missing-party vec "row ~a does not match the length of the first row (~a)" idx expected-len))
            (chaperone-vector
             row
             (lambda (r-vec r-idx val) (elem-proj val missing-party))
             (lambda (r-vec r-idx val) (elem-proj val missing-party)))
            row)
          (lambda (vec idx row)
            (unless (and (vector? row) (= (vector-length row) expected-len))
              (raise-blame-error blame #:missing-party missing-party vec "row ~a does not match the length of the first row (~a)" idx expected-len))
            row)))))

   #:generate
   (lambda (c)
     (lambda (fuel)
       (define element-thunk (contract-random-generate/choose (array-of-contract-val-ctc c) fuel))
       (lambda ()
         (define rows (add1 (random 5)))
         (define cols (add1 (random 5)))
         (build-vector rows (lambda (_)
                              (build-vector cols (lambda (_) (element-thunk))))))))))

(define (min-len-string/c min-len)
  (flat-named-contract
   (string->symbol (format "(min-len-string/c ~a)" min-len))
   (lambda (s) (and (string? s) (>= (string-length s) min-len)))
   (lambda (fuel)
     (define char-gen (contract-random-generate/choose char? fuel))
     (thunk
       (list->string
         (for/list ([i (in-range (+ min-len (random 10)))])
           (char-gen)))))))