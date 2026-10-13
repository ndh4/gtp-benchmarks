#lang racket

(require racket/contract
         "../../../ctcs/precision-config.rkt"
         (only-in "../../../ctcs/common.rkt" generate-small-eni))

(provide
  index?
  index/c
  size/c
  assert
)

(define/contract (assert v p)
  (configurable-ctc
   [max #;(parametric->/c [A] (A (A . -> . boolean?) . -> . A))
        (any/c (any/c . -> . boolean?) . -> . any/c)]
   [types (any/c (any/c . -> . boolean?) . -> . any/c)])

  (if (p v)
    v
    (raise-user-error 'assert)))

(define/contract (index? v)
  (configurable-ctc
   [max (any/c . -> . boolean?)]
   [types (any/c . -> . boolean?)])

  (and (exact-nonnegative-integer? v) (< v 9999999)))

(define/ctc-helper index/c
  (flat-named-contract
    'index/c
    (and/c exact-nonnegative-integer? (</c 9999999))
    (lambda (fuel) (thunk (generate-small-eni 15 9999999)))))

(define/ctc-helper size/c
  (flat-named-contract
    'index/c
    (and/c exact-nonnegative-integer? (>/c 0) (</c 9999999))
    (lambda (fuel) (thunk (add1 (generate-small-eni 5 3000))))))