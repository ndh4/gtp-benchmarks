#lang racket

(define (inverse a) (if (= a 0) 1000 (/ 1 a)))

#;(module+
 test
 (require rackunit)
 (check-equal? (inverse 0) 1000)
 (check-equal? (inverse 1) 1))

