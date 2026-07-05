#lang racket

(require
 ;; racket/contract
 racket/list
  "../../../ctcs/configurable.rkt"
 "../../../ctcs/precision-config.rkt"
 (only-in racket/function curry)
 (only-in "../../../ctcs/common.rkt"
          stack?
          list-with-min-size-two/c
          equal?/c))

(provide/configurable-contract
 [list->stack ([max (->i ([xs list?])
                         [result (xs) (equal?/c xs)])]
               [types (list? . -> . stack?)])]
 [stack-drop ([max (->i ([S non-empty-stack?])
                        [result (S) (equal?/c (rest S))])]
              [types (stack? . -> . stack?)])]
 [stack-dup ([max (->i ([S non-empty-stack?])
                       [result (S) (equal?/c (cons (first S) S))])]
             [types (stack? . -> . stack?)])]
 [stack-init ([max (-> (and/c stack? empty?))]
              [types (-> stack?)])]
 [stack-over ([max (->i ([S stack-with-min-size-two/c])
                        [result (S)
                                (equal?/c
                                 (cons (first S)
                                       (cons (second S)
                                             (cons (first S)
                                                   (rest (rest S))))))])]
              [types (stack? . -> . stack?)])]
 [stack-pop ([max (->i ([S non-empty-stack?])
                       (values
                        [first-result (S) (equal?/c (first S))]
                        [second-result (S) (equal?/c (rest S))]))]
             [types (stack? . -> . (values any/c stack?))])]
 [stack-push ([max (->i ([S stack?]
                         [v any/c])
                        [result (S v)
                                (equal?/c (cons v S))])]
              [types (stack? any/c . -> . stack?)])]
 [stack-swap ([max (->i ([S stack-with-min-size-two/c])
                        [result (S)
                                (equal?/c
                                 (cons (second S)
                                       (cons (first S)
                                             (rest (rest S)))))])]
              [types (stack? . -> . stack?)])])


;; TODO make stacks be objects ?

;; Forth stacks,
;;  data definition & operations

;; (provide
;;   ;; type Stack = List
;;   ;;
;;   ;; Notation:
;;   ;;  []    = empty stack
;;   ;;  x::xs = item 'x' on top of the stack 'xs'
;;   ;;
;;   ;; Operations will raise an exception with message "empty stack"
;;   ;;  if called on a stack with too few elements.

;;   stack-drop
;;   ;; (-> x::xs xs)
;;   ;; Drop the item on top of the stack

;;   stack-dup
;;   ;; (-> x::xs x::x::xs)
;;   ;; Duplicate the item on top of the stack

;;   stack-init
;;   ;; (-> Stack)
;;   ;; Initialize an empty stack

;;   stack-over
;;   ;; (-> x::y::xs x::y::x::xs)
;;   ;; Duplicate the item on top of the stack, but place the duplicate
;;   ;;  after the 2nd item on the stack.

;;   stack-pop
;;   ;; (-> x::xs (Values x xs))
;;   ;; Pop the top item from the stack, return both the item and the new stack.

;;   stack-push
;;   ;; (-> xs x x::xs)
;;   ;; Push an item on to the stack

;;   stack-swap
;;   ;; (-> x::y::xs y::x::xs)
;;   ;; Swap the positions of the first 2 items on the stack.
;; )

;; =============================================================================

(define/ctc-helper stackof listof)
(define/ctc-helper non-empty-stack? (and/c stack?
                                (not/c empty?)))
(define/ctc-helper stack-with-min-size-two/c list-with-min-size-two/c)

(define (list->stack xs)
  (for/fold ([S (stack-init)])
            ([x (in-list (reverse xs))])
    (stack-push S x)))

(define (stack-drop S)
  (let-values ([(_v S+) (stack-pop S)])
    S+))

(define (stack-dup S)
  (let-values ([(v S+) (stack-pop S)])
    (stack-push (stack-push S+ v) v)))

(define (stack-init)
  '())

(define (stack-over S)
  (let*-values ([(v1 S1) (stack-pop S)]
                [(v2 S2) (stack-pop S1)])
    (stack-push (stack-push (stack-push S2 v1) v2) v1)))

(define (stack-pop S)
  (if (null? S)
      (raise-user-error "empty stack")
      (values (car S) (cdr S))))

(define (stack-push S v)
  (cons v S))

(define (stack-swap S)
  (let*-values ([(v1 S1) (stack-pop S)]
                [(v2 S2) (stack-pop S1)])
    (stack-push (stack-push S2 v1) v2)))

;; TESTS

(module+ test

  (require rackunit)

  (define exn-rx #rx"empty stack")

;; Check-exn makes things complicated. In this case, adding contracts
;; to the program changes the type of exception that gets thrown (since contracts
;; may catch exceptional behavior before it happens, so now the exception
;; is coming from a contract-violation rather than a user branch.
;  (define-syntax-rule (check-stack-exn e)
;    (check-exn exn-rx (lambda () e)))

  (define S (list->stack '(1 2)))
  ;; -- drop
  (check-equal? (stack-drop S) (list->stack '(2)))
  (check-equal? (stack-drop (stack-drop S)) (list->stack '()))
  ;(check-stack-exn (stack-drop (stack-drop (stack-drop S))))
  ;; -- dup
  (check-equal? (stack-dup S) (list->stack '(1 1 2)))
  (check-equal? (stack-dup (stack-drop S)) (list->stack '(2 2)))
  ;(check-stack-exn (stack-dup (stack-init)))
  ;; -- init
  (check-equal? (stack-init) (list->stack '()))
  ;; -- over
  (check-equal? (stack-over S) (list->stack '(1 2 1)))
  ;(check-stack-exn (stack-over (stack-drop S)))
  ;(check-stack-exn (stack-over (stack-init)))
  ;; -- pop
  (let-values ([(v S2) (stack-pop S)])
    (check-equal? v 1)
    (check-equal? S2 (list->stack '(2))))
  ;(check-stack-exn (stack-pop (stack-drop (stack-drop S))))
  ;(check-stack-exn (stack-pop (stack-init)))
  ;; -- push
  (check-equal? (stack-push S 4) (list->stack '(4 1 2)))
  (check-equal? (stack-push (stack-init) 6) (list->stack '(6)))
  ;; -- swap
  (check-equal? (stack-swap S) (list->stack '(2 1)))
  (check-equal? (stack-swap (stack-swap S)) S)
  ;(check-stack-exn (stack-swap (stack-drop S)))
  ;(check-stack-exn (stack-swap (stack-init)))

  )