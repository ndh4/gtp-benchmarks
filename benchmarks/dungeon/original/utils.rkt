#lang racket

(require (only-in racket/list first permutations)
         (only-in racket/file file->value)
         racket/contract
         (only-in "../../../ctcs/common.rkt" memberof/c permutationof/c)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/configurable.rkt"
         "../base/random-number-table.rkt")

(provide orig r* reset! article random-between d6 d20 random-from shuffle)

(provide random-result-between/c)

(define/contract r* (configurable-ctc (max any/c) (types any/c)) (box orig))

(define/contract
 (reset!)
 (configurable-ctc
  (max (->* () void? #:post (equal? (unbox r*) orig)))
  (types (-> void?)))
 (set-box! r* orig))

(define/contract (random n)
  (configurable-ctc
   (max (->* ()
             void?
             #:post (equal? (unbox r*) orig)))
   (types (-> void?)))
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

(module+ test
  (require rackunit)

  ;; reset!
  (set-box! r* '(1 2 3 4 5))
  (reset!)
  (check-equal? (unbox r*) orig)

  ;; random
  (set-box! r* '(1 2 3 4 5))
  (check-equal? (random 3) 1)
  (check-equal? (random 123) 2)
  (check-equal? (random 1) 0)
  (check-equal? (random 3) 1)
  (check-equal? (random 3) 2)

  ;; random-between
  (set-box! r* '(1 2 3 4 5))
  (check-equal? (random-between 4 8) 5)
  (check-equal? (random-between 9 11) 9)
  (check-equal? (random-between 3 9) 6)
  (check-equal? (random-between 2 5) 3)
  (check-equal? (random-between 2 5) 4)

  ;; d6
  (set-box! r* '(15 20 25))
  (check-equal? (d6) 4)
  (check-equal? (d6) 3)
  (check-equal? (d6) 2)

  ;; d20
  (set-box! r* '(15 20 25))
  (check-equal? (d20) 16)
  (check-equal? (d20) 1)
  (check-equal? (d20) 6)

  ;; article
  (check-equal? (article #t #t) "The")
  (check-equal? (article #t #f) "A")
  (check-equal? (article #f #t) "the")
  (check-equal? (article #f #f) "a")
  (check-equal? (article #t #t #:an? #t) "The")
  (check-equal? (article #t #f #:an? #t) "An")
  (check-equal? (article #f #t #:an? #t) "the")
  (check-equal? (article #f #f #:an? #t) "an"))
