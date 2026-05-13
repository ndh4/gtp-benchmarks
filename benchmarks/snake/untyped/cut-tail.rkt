#lang racket

(require (except-in "data.rkt" posn=?)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require (only-in "data.rkt" posn=?))

(provide cut-tail)

(define/ctc-helper ne-segs? (and/c snake-segs? cons?))

(define/contract
 (cut-tail segs)
 (configurable-ctc
  (max
   (->i
    ((segs ne-segs?))
    (result (segs) (snake-segs=?/c (drop-right segs 1)))))
  (types (-> ne-segs? snake-segs?)))
 (let ((r (cdr segs)))
   (cond ((empty? r) empty) (else (cons (car segs) (cut-tail r))))))

#;(module+
 test
 (require rackunit)
 (check-equal? (cut-tail (list (posn 1 1))) (list))
 (check-equal? (cut-tail (list (posn 1 1) (posn 1 2))) (list (posn 1 1)))
 (check-equal?
  (cut-tail (list (posn 1 1) (posn 1 2) (posn 1 3) (posn 2 3)))
  (list (posn 1 1) (posn 1 2) (posn 1 3)))
 (check-equal?
  (cut-tail (list (posn 2 1) (posn 3 4) (posn 2 3)))
  (list (posn 2 1) (posn 3 4))))

