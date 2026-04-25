#lang racket

(provide/configurable-contract
 (LOOPS ((max 1) (types natural?)))
 (main ((max (-> (listof string?) void?)) (types (-> (listof string?) void?))))
 (lines ((max (listof string?)) (types (listof string?)))))

(require (only-in racket/file file->lines)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         (only-in racket/math natural?))

(require/configurable-contract "eval.rkt" forth-eval*)

(define LOOPS 1)

(define (main lines)
  (for
   ((i (in-range LOOPS)))
   (define-values (_e _s) (forth-eval* lines))
   (void)))

(define lines (file->lines "../base/history-100.txt"))

