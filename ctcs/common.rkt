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

(define (equal?/c c/v)
  (flat-named-contract
    (string->symbol (format "(equal?/c ~s)" c/v))
    (lambda (v) (equal? c/v v))))

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