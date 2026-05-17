#lang racket

(require (only-in racket/list first permutations)
         (only-in racket/file file->value)
         racket/contract
         (only-in "../../../ctcs/common.rkt" memberof/c permutationof/c)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/configurable.rkt"
         "../base/random-number-table.rkt")

(provide orig
         r*
         reset!
         random
         article
         random-between
         d6
         d20
         random-from
         shuffle)

(provide random-result-between/c)

(define/contract r* (configurable-ctc (max any/c) (types any/c)) (box orig))

(define/contract
 (reset!)
 (configurable-ctc
  (max (->* () void? #:post (equal? (unbox r*) orig)))
  (types (-> void?)))
 (set-box! r* orig))

(define/contract
 (random n)
 (configurable-ctc
  (max
   (->i
    ((n exact-nonnegative-integer?))
    (result (n) (and/c exact-nonnegative-integer? (</c n)))))
  (types (-> any/c exact-nonnegative-integer?)))
 (begin0 (modulo (car (unbox r*)) n) (set-box! r* (cdr (unbox r*)))))

(define/ctc-helper
 (list+titlecases . los)
 (append los (map string-titlecase los)))

(define/contract
 (article capitalize? specific? #:an? (an? #f))
 (configurable-ctc
  (max
   (->*
    (boolean? boolean?)
    (#:an? boolean?)
    (apply or/c (list+titlecases "the" "an" "a"))))
  (types (->* (boolean? boolean?) (#:an? boolean?) string?)))
 (if specific?
   (if capitalize? "The" "the")
   (if an? (if capitalize? "An" "an") (if capitalize? "A" "a"))))

(define/ctc-helper
 (random-result-between/c min max)
 (and/c exact-nonnegative-integer? (>=/c min) (<=/c max)))

(define/contract
 (random-between min max)
 (configurable-ctc
  (max
   (->i
    ((min exact-nonnegative-integer?)
     (max (min) (and/c exact-nonnegative-integer? (>/c min))))
    (result (min max) (random-result-between/c min max))))
  (types
   (->
    exact-nonnegative-integer?
    exact-nonnegative-integer?
    exact-nonnegative-integer?)))
 (+ min (random (- max min))))

(define/contract
 (d6)
 (configurable-ctc
  (max (-> (random-result-between/c 1 7)))
  (types (-> exact-nonnegative-integer?)))
 (random-between 1 7))

(define/contract
 (d20)
 (configurable-ctc
  (max (-> (random-result-between/c 1 21)))
  (types (-> exact-nonnegative-integer?)))
 (random-between 1 21))

(define/contract
 (random-from l)
 (configurable-ctc
  (max (->i ((l (listof any/c))) (result (l) (memberof/c l))))
  (types (-> (listof any/c) any/c)))
 (first (shuffle l)))

(define/contract
 (shuffle l)
 (configurable-ctc
  (max (->i ((l (listof any/c))) (result (l) (permutationof/c l))))
  (types (-> (listof any/c) (listof any/c))))
 (reverse l))

(module+
 test
 (require rackunit)
 (set-box! r* '(1 2 3 4 5))
 (reset!)
 (check-equal? (unbox r*) orig)
 (set-box! r* '(1 2 3 4 5))
 (define-values
  (r1 r2 r3 r4 r5)
  (values (random 3) (random 123) (random 1) (random 3) (random 3)))
 (check-equal? r1 1)
 (check-equal? r2 2)
 (check-equal? r3 0)
 (check-equal? r4 1)
 (check-equal? r5 2)
 (set-box! r* '(1 2 3 4 5))
 (define-values
  (rb1 rb2 rb3 rb4 rb5)
  (values
   (random-between 4 8)
   (random-between 9 11)
   (random-between 3 9)
   (random-between 2 5)
   (random-between 2 5)))
 (check-equal? rb1 5)
 (check-equal? rb2 9)
 (check-equal? rb3 6)
 (check-equal? rb4 3)
 (check-equal? rb5 4)
 (set-box! r* '(15 20 25))
 (define-values (d61 d62 d63) (values (d6) (d6) (d6)))
 (check-equal? d61 4)
 (check-equal? d62 3)
 (check-equal? d63 2)
 (set-box! r* '(15 20 25))
 (define-values (d201 d202 d203) (values (d20) (d20) (d20)))
 (check-equal? d201 16)
 (check-equal? d202 1)
 (check-equal? d203 6)
 (check-equal? (article #t #t) "The")
 (check-equal? (article #t #f) "A")
 (check-equal? (article #f #t) "the")
 (check-equal? (article #f #f) "a")
 (check-equal? (article #t #t #:an? #t) "The")
 (check-equal? (article #t #f #:an? #t) "An")
 (check-equal? (article #f #t #:an? #t) "the")
 (check-equal? (article #f #f #:an? #t) "an"))

