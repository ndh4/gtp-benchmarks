#lang racket

(provide
  (struct-out Stx)
  (struct-out useless-exp)
  (struct-out Ref)
  (struct-out Lam)
  (struct-out Call)
)

(provide Stx-type/c
         Exp-type/c
         Ref-type/c
         Lam-type/c
         Call-type/c
         Stx/c
         exp/c
         Ref/c
         Lam/c
         Call/c
         Var?

         Stx-label
         Ref?
         Ref-var
         Lam?
         Call?)

(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt")

(provide/configurable-contract)


;; =============================================================================

(define/ctc-helper Var? symbol?)

(struct Stx
 (label ;: Symbol]))
  )
  #:transparent)
(struct useless-exp Stx ()
  #:mutable
  #:transparent)

(struct Ref useless-exp
 (var ;: Symbol]))
)
  #:transparent)
(struct Lam useless-exp
 (formals ;: (Listof Symbol)]
  call ;: (U exp Ref Lam Call)]))
)
  #:transparent)

(struct Call Stx
 (fun ;: (U exp Ref Lam Call)]
  args ;: (Listof (U exp Ref Lam Call))]))
)
  #:transparent)


(define/ctc-helper (Stx/c label/c)
  (struct/c Stx label/c))
(define/ctc-helper (exp/c label/c)
  (struct/c useless-exp label/c))
(define/ctc-helper (Ref/c label/c var/c)
  (struct/c Ref label/c
            var/c))
(define/ctc-helper (Lam/c label/c formals/c call/c)
  (struct/c Lam label/c
            formals/c
            call/c))
(define/ctc-helper (Call/c label/c fun/c args/c)
  (struct/c Call label/c
            fun/c
            args/c))
(define/ctc-helper Stx-type/c (Stx/c symbol?))
(define/ctc-helper exp-type/c (exp/c symbol?))
(define/ctc-helper Ref-type/c (Ref/c symbol? Var?))
(define/ctc-helper Exp-type/c
  (or/c #;exp-type/c
        Ref-type/c
        (recursive-contract Lam-type/c #:flat)
        (recursive-contract Call-type/c #:flat)))
(define/ctc-helper Lam-type/c (struct/c Lam symbol?
                                        (listof Var?)
                                        Exp-type/c))
(define/ctc-helper Call-type/c (struct/c Call symbol?
                                         Exp-type/c
                                         (listof Exp-type/c)))
