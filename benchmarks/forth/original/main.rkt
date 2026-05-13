#lang racket

(provide LOOPS main lines)

(require (only-in racket/file file->lines)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         (only-in racket/math natural?))

(require (only-in "eval.rkt" forth-eval*))

(define/contract LOOPS (configurable-ctc (max 1) (types natural?)) 1)

(define/contract
 (main lines)
 (configurable-ctc
  (max (-> (listof string?) void?))
  (types (-> (listof string?) void?)))
 (for
  ((i (in-range LOOPS)))
  (define-values (_e _s) (forth-eval* lines))
  (void)))

(define/contract
 lines
 (configurable-ctc (max (listof string?)) (types (listof string?)))
 (file->lines "../base/history-100.txt"))

