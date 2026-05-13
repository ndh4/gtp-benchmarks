#lang racket

(require racket/list
         "structs.rkt"
         (only-in
          "benv.rkt"
          Closure
          Binding
          BEnv?
          Closure/c
          Binding/c
          Closure-type/c
          Binding-type/c
          Time?
          Addr?
          Closure-lam
          Closure-benv
          Binding-var
          Binding-time)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt")

(require (only-in "benv.rkt" benv-extend* benv-extend benv-lookup empty-benv))

(provide take* time-zero tick alloc)

(define/ctc-helper natural? exact-nonnegative-integer?)

(define/ctc-helper ((length-is/c compare) n) (λ (l) (compare (length l) n)))

(define/ctc-helper length<=/c (length-is/c <=))

(define/ctc-helper length=/c (length-is/c =))

(define/ctc-helper ((prefix-of/c l) pref) (list-prefix? pref l))

(define/contract
 (take* l n)
 (configurable-ctc
  (max
   (->i
    ((l (listof any/c)) (n natural?))
    (result (l n) (and/c (listof any/c) (length<=/c n) (prefix-of/c l)))))
  (types (-> (listof any/c) natural? (listof any/c))))
 (for/list ((e (in-list l)) (i (in-range n))) e))

(define/contract
 time-zero
 (configurable-ctc (max (listof Time?)) (types (listof Time?)))
 '())

(define/contract
 (tick call time)
 (configurable-ctc
  (max
   (->i
    ((call Stx-type/c) (time Time?))
    (result
     (call time)
     (and/c Time? (length=/c 1) (prefix-of/c (cons (Stx-label call) time))))))
  (types (-> Stx-type/c Time? Time?)))
 (define label (Stx-label call))
 (take* (cons label time) 1))

(define/contract
 (alloc time)
 (configurable-ctc
  (max
   (->i
    ((time Time?))
    (result
     (time)
     (->i
      ((var Var?))
      (result
       (var)
       (and/c Binding-type/c (Binding/c (equal?/c var) (equal?/c time))))))))
  (types (-> Time? (-> Var? Binding-type/c))))
 (λ (var) (Binding var time)))

(module+
 test
 (require rackunit (only-in racket/format ~a))
 (check-equal? (take* '(a b c d e f) 2) '(a b))
 (check-equal? (take* '(a b c d e) 3) '(a b c))
 (check-equal? time-zero '())
 (check-equal? (tick (Stx 'g123) time-zero) '(g123))
 (check-equal? (tick (Stx 'g123) '(g123)) '(g123))
 (check-equal? ((alloc '(g123)) 'x) (Binding 'x '(g123))))

