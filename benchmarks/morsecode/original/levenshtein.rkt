#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         racket/match
         racket/math)

(provide %identity
         %string-empty?
         %vector-empty?
         %string->vector
         vector-levenshtein/predicate/get-scratch
         vector-levenshtein/predicate
         vector-levenshtein/eq
         vector-levenshtein/eqv
         vector-levenshtein/equal
         vector-levenshtein
         list-levenshtein/predicate
         list-levenshtein/eq
         list-levenshtein/eqv
         list-levenshtein/equal
         list-levenshtein
         string-levenshtein
         %string-levenshtein/predicate
         levenshtein)

(define/contract
 (%identity x)
 (configurable-ctc (max (-> any/c any/c)) (types (-> any/c any/c)))
 x)

(define/contract
 (%string-empty? v)
 (configurable-ctc
  (max (->i ((v string?)) (result (v) (zero? (string-length v)))))
  (types (-> string? boolean?)))
 (zero? (string-length v)))

(define/contract
 (%vector-empty? v)
 (configurable-ctc
  (max (->i ((v my-vector?)) (result (v) (zero? (vector-length v)))))
  (types (-> my-vector? boolean?)))
 (zero? (vector-length v)))

(define/contract
 (%string->vector s)
 (configurable-ctc
  (max
   (->i
    ((s string?))
    (result (vectorof char?))
    #:post
    (s result)
    (and (= (string-length s) (vector-length result))
         (andmap char=? (string->list s) (vector->list result)))))
  (types (-> string? (vectorof char?))))
 (list->vector (string->list s)))

(define/ctc-helper
 (editable-to?/DP a b edits #:compare-with elt-equal?)
 (let* ((a-length (vector-length a))
        (b-length (vector-length b))
        (memo-table
         (make-vector (* (add1 a-length) (add1 b-length) (add1 edits)) #f))
        (i+j+e->index
         (λ (i j e)
           (+ (* i (add1 b-length) (add1 edits)) (* j (add1 edits)) e)))
        (memo-set!
         (λ (i j e v) (vector-set! memo-table (i+j+e->index i j e) v)))
        (memo-ref (λ (i j e) (vector-ref memo-table (i+j+e->index i j e)))))
   (for*
    ((a-index (in-range (vector-length a) -1 -1))
     (b-index (in-range (vector-length b) -1 -1))
     (edits (in-range (add1 edits))))
    (memo-set!
     a-index
     b-index
     edits
     (cond
      ((and (>= a-index (vector-length a)) (>= b-index (vector-length b)))
       (>= edits 0))
      ((>= a-index (vector-length a)) (<= (- b-length b-index) edits))
      ((>= b-index (vector-length b)) (<= (- a-length a-index) edits))
      (else
       (or (and (elt-equal? (vector-ref a a-index) (vector-ref b b-index))
                (memo-ref (add1 a-index) (add1 b-index) edits))
           (and (> edits 0)
                (or (memo-ref (add1 a-index) b-index (sub1 edits))
                    (memo-ref a-index (add1 b-index) (sub1 edits))
                    (memo-ref
                     (add1 a-index)
                     (add1 b-index)
                     (sub1 edits)))))))))
   (memo-ref 0 0 edits)))

(define/ctc-helper cache (make-hash))

(define/ctc-helper
 editable-to?/DP/memo
 (λ (a b edits #:compare-with elt-equal?)
   (define key (list a b edits elt-equal?))
   (cond
    ((hash-has-key? cache key) (hash-ref cache key))
    (else
     (define r (editable-to?/DP a b edits #:compare-with elt-equal?))
     (hash-set! cache key r)
     r))))

(define/ctc-helper
 (editable-to? a b edits #:compare-with elt-equal?)
 (define a/vec (for/vector ((el a)) el))
 (define b/vec (for/vector ((el b)) el))
 (define length-difference
   (abs (- (vector-length a/vec) (vector-length b/vec))))
 (and (>= edits length-difference)
      (if (<= edits 0)
        (for/and
         ((a-el (in-vector a/vec)) (b-el (in-vector b/vec)))
         (elt-equal? a-el b-el))
        (editable-to?/DP/memo a/vec b/vec edits #:compare-with elt-equal?))))

(define/ctc-helper
 get-scratch/c
 (make-contract
  #:name
  'get-scratch/c
  #:late-neg-projection
  (contract-late-neg-projection
   (->i
    ((n natural?))
    (result (and/c my-vector? (not/c immutable?)))
    #:post
    (n result)
    (= (vector-length result) n)))
  #:generate
  (λ (fuel)
    (define any-generator (contract-random-generate/choose any/c fuel))
    (define eni-function-gen
      (contract-random-generate/choose
       (-> exact-nonnegative-integer? any/c)
       fuel))
    (lambda ()
      (define choices
        (list
         make-vector
         (lambda (size) (make-vector size (any-generator)))
         (lambda (size) (build-vector size (eni-function-gen)))))
      (get-random-element choices)))))

(define/contract
 (vector-levenshtein/predicate/get-scratch a b pred get-scratch)
 (configurable-ctc
  (max
   (->i
    ((a my-vector?)
     (b my-vector?)
     (pred commutative-binary-predicate?)
     (get-scratch get-scratch/c))
    (result natural?)
    #:post
    (a b pred result)
    (editable-to? a b result #:compare-with pred)))
  (types
   (->
    my-vector?
    my-vector?
    (-> any/c any/c boolean?)
    (-> natural? my-vector?)
    natural?)))
 (let ((a-len (vector-length a)) (b-len (vector-length b)))
   (cond
    ((zero? a-len) b-len)
    ((zero? b-len) a-len)
    (else
     (let ((w (get-scratch (+ 1 b-len))) (next #f))
       (let fill ((k b-len)) (vector-set! w k k) (or (zero? k) (fill (- k 1))))
       (let loop-i ((i 0))
         (if (= i a-len)
           next
           (let ((a-i (vector-ref a i)))
             (let loop-j ((j 0) (cur (+ 1 i)))
               (if (= j b-len)
                 (begin (vector-set! w b-len next) (loop-i (+ 1 i)))
                 (begin
                   (set! next
                     (min
                      (+ 1 (vector-ref w (+ 1 j)))
                      (+ 1 cur)
                      (if (pred a-i (vector-ref b j))
                        (vector-ref w j)
                        (+ 1 (vector-ref w j)))))
                   (vector-set! w j cur)
                   (loop-j (+ 1 j) next))))))))))))

(module+
 test
 (require rackunit)
 (check-equal? (vector-levenshtein '#(6 6 6) '#(6 35 6 24 6 32)) 3))

(define/ctc-helper
 (make-fixed-or/c*)
 (define generator-store (box #f))
 (define predicate-store (box #f))
 (define (clean-predicate-store!? . _) (set-box! predicate-store #f) #t)
 (define ctc
   (make-contract
    #:name
    (string->symbol "(fixed-or/c string? my-vector? list?)")
    #:late-neg-projection
    (λ (blame)
      (λ (val neg-party)
        (define stored-type (unbox predicate-store))
        (match
         stored-type
         ((? symbol?)
          (define ctc-pred
            (case stored-type
              ((string?) string?)
              ((my-vector?) my-vector?)
              ((list?) list?)))
          (((contract-late-neg-projection ctc-pred) blame) val neg-party))
         (#f
          (match
           val
           ((? string?) (set-box! predicate-store 'string?) val)
           ((? vector?) (set-box! predicate-store 'my-vector?) val)
           ((? list?) (set-box! predicate-store 'list?) val)
           (else
            (raise-blame-error
             blame
             #:missing-party
             neg-party
             val
             '(expected "(or/c string? my-vector? list?)" given: "~e")
             val)))))))
    #:generate
    (λ (fuel)
      (define string-generator (contract-random-generate/choose string? fuel))
      (define list-generator (contract-random-generate/choose list? fuel))
      (define vector-generator (lambda _ (list->vector (list-generator))))
      (lambda _
        (define stored-type (unbox generator-store))
        (match
         stored-type
         ((? symbol?)
          (set-box! generator-store #f)
          (case stored-type
            ((string?) (string-generator))
            ((my-vector?) (vector-generator))
            ((list?) (list-generator))))
         (#f
          (case (random 3)
            ((0) (set-box! generator-store 'string?) (string-generator))
            ((1) (set-box! generator-store 'my-vector?) (vector-generator))
            ((2) (set-box! generator-store 'list?) (list-generator)))))))))
 (cons ctc clean-predicate-store!?))

(define/ctc-helper fixed-vsl/c** (make-fixed-or/c*))

(define/ctc-helper fixed-vsl/c (car fixed-vsl/c**))

(define/ctc-helper reset-vsl!? (cdr fixed-vsl/c**))

(define/ctc-helper
 (levenshtein-variant/pred/c #:at level #:sequence-type (seq? my-vector?))
 (match
  level
  ('max
   (->i
    ((a seq?) (b seq?) (pred commutative-binary-predicate?))
    (result natural?)
    #:post
    (a b pred result)
    (editable-to? a b result #:compare-with pred)))
  ('types (-> seq? seq? (-> any/c any/c boolean?) natural?))))

(define/ctc-helper
 (levenshtein-variant/c
  pred
  #:at
  level
  #:sequence-type
  (seq? my-vector?)
  #:clean-seqtype-env!?
  (clean-seqtype-env!? (lambda () (void))))
 (match
  level
  ('max
   (->i
    ((a seq?) (b seq?))
    (result natural?)
    #:post
    (a b result)
    (and (editable-to? a b result #:compare-with pred) (reset-vsl!?))))
  ('types (-> seq? seq? natural?))))

(define/contract
 (vector-levenshtein/predicate a b pred)
 (configurable-ctc
  (max (levenshtein-variant/pred/c #:at 'max))
  (types (levenshtein-variant/pred/c #:at 'types)))
 (vector-levenshtein/predicate/get-scratch a b pred make-vector))

(define/contract
 (vector-levenshtein/eq a b)
 (configurable-ctc
  (max (levenshtein-variant/c eq? #:at 'max))
  (types (levenshtein-variant/c eq? #:at 'types)))
 (vector-levenshtein/predicate a b eq?))

(define/contract
 (vector-levenshtein/eqv a b)
 (configurable-ctc
  (max (levenshtein-variant/c eqv? #:at 'max))
  (types (levenshtein-variant/c eqv? #:at 'types)))
 (vector-levenshtein/predicate a b eqv?))

(define/contract
 (vector-levenshtein/equal a b)
 (configurable-ctc
  (max (levenshtein-variant/c equal? #:at 'max))
  (types (levenshtein-variant/c equal? #:at 'types)))
 (vector-levenshtein/predicate a b equal?))

(define/contract
 (vector-levenshtein a b)
 (configurable-ctc
  (max (levenshtein-variant/c equal? #:at 'max))
  (types (levenshtein-variant/c equal? #:at 'types)))
 (vector-levenshtein/equal a b))

(module+
 test
 (require rackunit)
 (check-equal? (list-levenshtein/eq '(b c e x f y) '(a b c d e f)) 4))

(define/contract
 (list-levenshtein/predicate a b pred)
 (configurable-ctc
  (max (levenshtein-variant/pred/c #:at 'max #:sequence-type list?))
  (types (levenshtein-variant/pred/c #:at 'types #:sequence-type list?)))
 (cond
  ((null? a) (length b))
  ((null? b) (length a))
  (else
   (vector-levenshtein/predicate (list->vector a) (list->vector b) pred))))

(define/contract
 (list-levenshtein/eq a b)
 (configurable-ctc
  (max (levenshtein-variant/c eq? #:at 'max #:sequence-type list?))
  (types (levenshtein-variant/c eq? #:at 'types #:sequence-type list?)))
 (list-levenshtein/predicate a b eq?))

(define/contract
 (list-levenshtein/eqv a b)
 (configurable-ctc
  (max (levenshtein-variant/c eqv? #:at 'max #:sequence-type list?))
  (types (levenshtein-variant/c eqv? #:at 'types #:sequence-type list?)))
 (list-levenshtein/predicate a b eqv?))

(define/contract
 (list-levenshtein/equal a b)
 (configurable-ctc
  (max (levenshtein-variant/c equal? #:at 'max #:sequence-type list?))
  (types (levenshtein-variant/c equal? #:at 'types #:sequence-type list?)))
 (list-levenshtein/predicate a b equal?))

(define/contract
 (list-levenshtein a b)
 (configurable-ctc
  (max (levenshtein-variant/c equal? #:at 'max #:sequence-type list?))
  (types (levenshtein-variant/c equal? #:at 'types #:sequence-type list?)))
 (list-levenshtein/equal a b))

(module+
 test
 (require rackunit)
 (check-equal? (string-levenshtein "adresse" "address") 2))

(define/contract
 (string-levenshtein a b)
 (configurable-ctc
  (max (levenshtein-variant/c eqv? #:at 'max #:sequence-type string?))
  (types (levenshtein-variant/c eqv? #:at 'types #:sequence-type string?)))
 (cond
  ((zero? (string-length a)) (string-length b))
  ((zero? (string-length b)) (string-length a))
  (else (vector-levenshtein/eqv (%string->vector a) (%string->vector b)))))

(define/contract
 (%string-levenshtein/predicate a b pred)
 (configurable-ctc
  (max (levenshtein-variant/pred/c #:at 'max #:sequence-type string?))
  (types (levenshtein-variant/pred/c #:at 'types #:sequence-type string?)))
 (cond
  ((zero? (string-length a)) (string-length b))
  ((zero? (string-length b)) (string-length a))
  (else
   (vector-levenshtein/predicate
    (%string->vector a)
    (%string->vector b)
    pred))))

(module+
 test
 (require rackunit)
 (check-equal? (%string-levenshtein/predicate "ABCD" "aBXcD" char-ci=?) 1)
 (check-equal?
  (vector-levenshtein/predicate
   #(#\A #\B #\C #\D)
   #(#\a #\B #\X #\c #\D)
   char-ci=?)
  1)
 (check-equal?
  (list-levenshtein/predicate
   '(#\A #\B #\C #\D)
   '(#\a #\B #\X #\c #\D)
   char-ci=?)
  1))

(module+
 test
 (require rackunit)
 (define g "gumbo")
 (check-equal? (levenshtein g "gambol") 2)
 (check-equal? (levenshtein g "dumbo") 1)
 (check-equal? (levenshtein g "umbrage") 5))

(define/contract
 (levenshtein a b)
 (configurable-ctc
  (max
   (levenshtein-variant/c
    equal?
    #:at
    'max
    #:sequence-type
    fixed-vsl/c
    #:clean-seqtype-env!?
    reset-vsl!?))
  (types
   (levenshtein-variant/c
    equal?
    #:at
    'types
    #:sequence-type
    (or/c string? my-vector? list?))))
 (cond
  ((and (string? a) (string? b)) (string-levenshtein a b))
  ((and (vector? a) (vector? b)) (vector-levenshtein a b))
  ((and (list? a) (list? b)) (list-levenshtein a b))
  (else (error "levenshtein"))))

