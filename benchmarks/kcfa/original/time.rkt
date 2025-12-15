#lang racket

(require
 racket/list
  "structs.rkt"
  (only-in "benv.rkt" Closure Binding BEnv? Closure/c Binding/c Closure-type/c Binding-type/c
           Time? Addr? Closure-lam Closure-benv Binding-var Binding-time)
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  "../../../ctcs/configurable.rkt"
  )
(require/configurable-contract "benv.rkt" benv-extend* benv-extend benv-lookup empty-benv )

;; ---

(provide/configurable-contract
 [take* ([max (->i ([l (listof any/c)]
                    [n natural?])
                   [result (l n)
                           (and/c (listof any/c)
                                  (length<=/c n)
                                  (prefix-of/c l))])]
         [types ((listof any/c) natural? . -> . (listof any/c))])]
 [time-zero ([max (listof Time?)]
             [types (listof Time?)])]
 #;[k ([max (parameter/c (and/c natural?
                              (=/c 1)))]
     [types (parameter/c natural?)])]
 [tick ([max (->i ([call Stx-type/c]
                   [time Time?])
                  [result (call time)
                          (and/c Time?
                                 (length=/c 1 #;(k))
                                 (prefix-of/c (cons (Stx-label call) time)))])]
        [types (Stx-type/c Time? . -> . Time?)])]
 [alloc ([max (->i ([time Time?])
                   [result (time)
                           (->i ([var Var?])
                                [result (var)
                                        (and/c Binding-type/c
                                               (Binding/c (equal?/c var)
                                                          (equal?/c time)))])])]
         [types (Time? . -> . (Var? . -> . Binding-type/c))])])


;; (provide
;;   time-zero
;;   ;; k
;;   tick
;;   alloc
;; )

;; =============================================================================

(define/ctc-helper natural? exact-nonnegative-integer?)

;; ---
;(define-type Value Closure)

(define/ctc-helper ((length-is/c compare) n)
  ;; ll: we only implemented up to one level of currying :(
  (λ (l) (compare (length l) n)))
(define/ctc-helper length<=/c (length-is/c <=))
(define/ctc-helper length=/c (length-is/c =))
(define/ctc-helper ((prefix-of/c l) pref)
  (list-prefix? pref l))

;(: take* (All (A) (-> (Listof A) Natural (Listof A))))
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-range n)])
    e))

;; ---

;(: time-zero Time)
(define time-zero
  '())

;(: k (Parameterof Natural))
#;(define k
  (make-parameter 1))

;(: tick (-> Stx Time Time))
(define (tick call time)
  (define label (Stx-label call))
  (take* (cons label time) 1 #;(k)))

;(: alloc (-> Time (-> Var Addr)))
(define (alloc time)
  (λ (var)
    (Binding var time)))

