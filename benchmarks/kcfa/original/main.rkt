#lang racket
;; Create a few examples and run abstract interpretation

(require
  "structs.rkt"
  ;; "ui.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  "../../../ctcs/configurable.rkt"
  (only-in "ai.rkt" closed-term?)
  )
(require/configurable-contract "ui.rkt" format-mono-store analyze monovariant-store monovariant-value empty-mono-store summarize )

;; (provide/configurable-contract
;;  [new-label any/c]
;;  [make-ref ([max (->i ([var Var?])
;;              [result (var)
;;                      (and/c Ref-type/c
;;                             (Ref/c symbol? (equal?/c var)))])]
;;    [types (Var? . -> . Ref-type/c)])]
;;  [make-lambda ([max (->i ([formals (listof Var?)]
;;               [call Exp-type/c])
;;              [result (formals call)
;;                      (and/c Lam-type/c
;;                             (Lam/c symbol?
;;                                    (equal?/c formals)
;;                                    (equal?/c call)))])]
;;    [types ((listof Var?) Exp-type/c . -> . Lam-type/c)])]
;;  [make-call ([max (->i ([fun Exp-type/c])
;;              #:rest [args (listof Exp-type/c)]
;;              [result (fun args)
;;                      (and/c Call-type/c
;;                             (Call/c symbol?
;;                                     (equal?/c fun)
;;                                     (equal?/c args)))])]
;;    [types (Exp-type/c Exp-type/c ... . -> . Call-type/c)])]
;;  [make-let ([max (->i ([var Var?]
;;               [exp Exp-type/c]
;;               [call Call-type/c])
;;              [result (var exp call)
;;                      (and/c Call-type/c
;;                             (Call/c symbol?
;;                                     (Lam/c symbol?
;;                                            (list/c (equal?/c var))
;;                                            (equal?/c call))
;;                                     (list/c (equal?/c exp))))])]
;;    [types (Var? Exp-type/c Call-type/c . -> . Call-type/c)])]
;;  [standard-example ([max (and/c Call-type/c closed-term?)]
;;    [types Call-type/c])]
;;  [main (-> (and/c Call-type/c closed-term?) any)])


;; =============================================================================

(module+ test
  (require
    rackunit
    (only-in racket/format ~a))

  ;; Use interned symbols here for testing purposes (we want to be able to compare them)
  (define make-fake-gensym
    (lambda ()
      (let ([counter 0])
        (lambda ([x 'g])
          (define new-sym
            (string->symbol (format "~a~a" x counter)))
          (set! counter (add1 counter))
          new-sym))))
  
  (define new-label
    (make-fake-gensym))

  ;(: make-ref (-> Var Exp))
  (define (make-ref var)
    (Ref (new-label) var))

  ;(: make-lambda (-> (Listof Var) Exp Exp))
  (define (make-lambda formals call)
    (Lam (new-label) formals call))

  ;(: make-call (-> Exp Exp * Exp))
  (define (make-call fun . args)
    (Call (new-label) fun args))

  (define (make-let var exp call)
    (make-call (make-lambda (list var) call) exp))

  (define smallest-example (make-lambda '(a) (make-ref 'a)))

  (check-equal? (analyze smallest-example) (hash))

  (define tiny-example
    (make-call (make-lambda '(a) (make-ref 'a))
               (make-lambda '(b) (make-ref 'b))))

  (check-equal? (analyze tiny-example)
                (hash 'a (set (Lam 'g5 '(b) (Ref 'g4 'b)))))

  (define standard-example
    (make-let
     'id
     (make-lambda '(x k) (make-call (make-ref 'k) (make-ref 'x)))
     (make-call (make-ref 'id)
                (make-lambda '(z) (make-ref 'z))
                (make-lambda '(a)
                             (make-call (make-ref 'id)
                                        (make-lambda '(y) (make-ref 'y))
                                        (make-lambda '(b)
                                                     (make-ref 'b)))))))

  (check-equal? (analyze standard-example)
                (hash
                 'a
                 (set (Lam 'g13 '(z) (Ref 'g12 'z)))
                 'b
                 (set (Lam 'g16 '(y) (Ref 'g15 'y)))
                 'id
                 (set (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x)))))
                 'k
                 (set
                  (Lam
                   'g20
                   '(a)
                   (Call
                    'g19
                    (Ref 'g14 'id)
                    (list (Lam 'g16 '(y) (Ref 'g15 'y)) (Lam 'g18 '(b) (Ref 'g17 'b)))))
                  (Lam 'g18 '(b) (Ref 'g17 'b)))
                 'x
                 (set (Lam 'g13 '(z) (Ref 'g12 'z)) (Lam 'g16 '(y) (Ref 'g15 'y))))))