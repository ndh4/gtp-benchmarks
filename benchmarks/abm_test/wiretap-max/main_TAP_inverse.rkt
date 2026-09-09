#lang racket

(require (for-syntax racket/base))
(define-syntax ctc-level 'max)

(require "../../../wiretapping/wiretap.rkt")

(require "../../../ctcs/configurable.rkt" "../../../ctcs/precision-config.rkt")

(define/contract
 (inverse a)
 (add-arg-recorder
  (configurable-ctc (max (-> integer? number?)) (types (-> integer? number?))))
 (if (= a 0) 1000 (/ 1 a)))

(eprintf "(begin-random-tests)~n~n")
(for ((fuel (in-range 10))) (contract-exercise inverse #:fuel fuel))
