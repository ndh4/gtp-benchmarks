#lang racket

(define (plus a b)
  (if (= a 0)
      b
      (+ a b)))

(module+ test
  (require rackunit)
  (check-equal? (plus 1 2) 3))
