#lang racket

(require (for-syntax racket/base))
(define-syntax ctc-level 'max)

(require "../../../ctcs/configurable.rkt" "../../../ctcs/precision-config.rkt")

(define/contract
 (inverse a)
 (configurable-ctc (max (-> integer? number?)) (types (-> integer? number?)))
 (if (= a 0) 1000 (/ 1 a)))

(module+
 test
 (require rackunit)
 (check-equal? (inverse 0) 1000)
 (check-equal? (inverse 1) 1))
