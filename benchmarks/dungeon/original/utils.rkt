#lang racket

(require
  (only-in racket/list first permutations)
  (only-in racket/file file->value)
  racket/contract
  (only-in "../../../ctcs/common.rkt"
           memberof/c
           permutationof/c)
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/configurable.rkt"
  "../base/random-number-table.rkt"
)

(provide/configurable-contract
 [orig ([max any/c]
        [types any/c])]
 [r* ([max any/c]
      [types any/c])]
 [reset! ([max (->* ()
                    void?
                    #:post (equal? (unbox r*) orig))]
          [types (-> void?)])]
 #;[random ([max (->i ([n exact-nonnegative-integer?])
                    [result (n) (and/c exact-nonnegative-integer?
                                       (</c n))])]
          [types (any/c . -> . exact-nonnegative-integer?)])]
 [article ([max (->* (boolean? boolean?)
                     [#:an? boolean?]
                     (apply or/c (list+titlecases "the" "an" "a")))]
           [types (->* (boolean? boolean?)
                       [#:an? boolean?]
                       string?)])]
 [random-between ([max (->i ([min exact-nonnegative-integer?]
                             [max (min) (and/c exact-nonnegative-integer?
                                               (>/c min))])
                            [result (min max) (random-result-between/c min max)])]
                  [types (exact-nonnegative-integer? exact-nonnegative-integer?
                                                     . -> . exact-nonnegative-integer?)])]
 [d6 ([max (-> (random-result-between/c 1 7))]
      [types (-> exact-nonnegative-integer?)])]
 [d20 ([max (-> (random-result-between/c 1 21))]
       [types (-> exact-nonnegative-integer?)])]
 [random-from ([max (->i ([l (listof any/c)])
                         [result (l) (memberof/c l)])]
               [types ((listof any/c) . -> . any/c)])]
 [shuffle ([max (->i ([l (listof any/c)])
                     [result (l) (permutationof/c l)])]
           [types ((listof any/c) . -> . (listof any/c))])])

(provide
;;   article
;;   random-between
;;   d6
;;   d20
;;   random-from
;;   random
;;   reset!
  random-result-between/c
)

;; =============================================================================

(define r* (box orig))

(define (reset!)
  (set-box! r* orig))

;; Non-specific ctc because this random stuff is rigged to be deterministic
(define (random n)
  (begin0 (car (unbox r*)) (set-box! r* (cdr (unbox r*)))))

(define/ctc-helper (list+titlecases . los)
  (append los
          (map string-titlecase los)))

(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))


(define/ctc-helper (random-result-between/c min max)
  (and/c exact-nonnegative-integer?
         (>=/c min)
         (<=/c max)))

(define (random-between min max) ;; TODO replace with 6.4's `random`
  (+ min (random (- max min))))

(define (d6)
  (random-between 1 7))

(define (d20)
  (random-between 1 21))

(define (random-from l)
  (first (shuffle l)))

(define (shuffle l)
  (reverse l))
