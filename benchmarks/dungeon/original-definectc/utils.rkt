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

(define (random n) (begin0 (car (unbox r*)) (set-box! r* (cdr (unbox r*)))))

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

