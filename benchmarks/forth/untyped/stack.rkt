#lang racket

(require racket/list
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         (only-in racket/function curry)
         (only-in
          "../../../ctcs/common.rkt"
          stack?
          list-with-min-size/c
          equal?/c))

(provide/configurable-contract
 (list->stack
  ((max (->i ((xs list?)) (result (xs) (equal?/c xs))))
   (types (-> list? stack?))))
 (stack-drop
  ((max (->i ((S non-empty-stack?)) (result (S) (equal?/c (rest S)))))
   (types (-> stack? stack?))))
 (stack-dup
  ((max
    (->i ((S non-empty-stack?)) (result (S) (equal?/c (cons (first S) S)))))
   (types (-> stack? stack?))))
 (stack-init ((max (-> (and/c stack? empty?))) (types (-> stack?))))
 (stack-over
  ((max
    (->i
     ((S (stack-with-min-size/c 2)))
     (result
      (S)
      (equal?/c
       (cons (first S) (cons (second S) (cons (first S) (rest (rest S)))))))))
   (types (-> stack? stack?))))
 (stack-pop
  ((max
    (->i
     ((S stack?))
     (values
      (first-result (S) (equal?/c (first S)))
      (second-result (S) (equal?/c (rest S))))))
   (types (-> stack? (values any/c stack?)))))
 (stack-push
  ((max (->i ((S stack?) (v any/c)) (result (S v) (equal?/c (cons v S)))))
   (types (-> stack? any/c stack?))))
 (stack-swap
  ((max
    (->i
     ((S (stack-with-min-size/c 2)))
     (result
      (S)
      (equal?/c (cons (second S) (cons (first S) (rest (rest S))))))))
   (types (-> stack? stack?)))))

(define/ctc-helper stackof listof)

(define/ctc-helper non-empty-stack? (and/c stack? (not/c empty?)))

(define/ctc-helper stack-with-min-size/c list-with-min-size/c)

(define (list->stack xs)
  (for/fold ((S (stack-init))) ((x (in-list (reverse xs)))) (stack-push S x)))

(define (stack-drop S) (let-values (((_v S+) (stack-pop S))) S+))

(define (stack-dup S)
  (let-values (((v S+) (stack-pop S))) (stack-push (stack-push S+ v) v)))

(define (stack-init) '())

(define (stack-over S)
  (let*-values (((v1 S1) (stack-pop S)) ((v2 S2) (stack-pop S1)))
    (stack-push (stack-push (stack-push S2 v1) v2) v1)))

(define (stack-pop S)
  (if (null? S) (raise-user-error "empty stack") (values (car S) (cdr S))))

(define (stack-push S v) (cons v S))

(define (stack-swap S)
  (let*-values (((v1 S1) (stack-pop S)) ((v2 S2) (stack-pop S1)))
    (stack-push (stack-push S2 v1) v2)))

#;(module+
 test
 (require rackunit)
 (define exn-rx #rx"empty stack")
 (define S (list->stack '(1 2)))
 (check-equal? (stack-drop S) (list->stack '(2)))
 (check-equal? (stack-drop (stack-drop S)) (list->stack '()))
 (check-equal? (stack-dup S) (list->stack '(1 1 2)))
 (check-equal? (stack-dup (stack-drop S)) (list->stack '(2 2)))
 (check-equal? (stack-init) (list->stack '()))
 (check-equal? (stack-over S) (list->stack '(1 2 1)))
 (let-values (((v S2) (stack-pop S)))
   (check-equal? v 1)
   (check-equal? S2 (list->stack '(2))))
 (check-equal? (stack-push S 4) (list->stack '(4 1 2)))
 (check-equal? (stack-push (stack-init) 6) (list->stack '(6)))
 (check-equal? (stack-swap S) (list->stack '(2 1)))
 (check-equal? (stack-swap (stack-swap S)) S))

