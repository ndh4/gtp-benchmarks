# Q: Why did snake’s no-contract mutation score go down?  
  
A: Before, we were using the ****provide/configurable-contract**** macro, which creates a submodule ****contract-maps****.  
  
File test2.rkt:  
```
#lang racket
(define-syntax ctc-level 'none)
(require "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(provide/configurable-contract
          [GRID-SIZE ([max natural?])]
          [BOARD-HEIGHT ([max natural?])]
          [BOARD-HEIGHT-PIXELS
           ([max (-> (and/c natural? (=/c (* GRID-SIZE BOARD-HEIGHT))))])]) ;; This multiplication will throw an error if evaluated.

(define GRID-SIZE #f) ;; Mutation. This is supposed to be a number, not #f.
(define BOARD-HEIGHT 1)
(define
  (BOARD-HEIGHT-PIXELS)
  (* GRID-SIZE BOARD-HEIGHT))

```
  
++→  [Macro transformation]++  
  
```
…
   (begin
     (provide (contract-out
               [GRID-SIZE any/c]
               [BOARD-HEIGHT any/c]
               [BOARD-HEIGHT-PIXELS any/c]))
     (module+
      contract-maps
      (provide contract-maps)
      (define contract-maps (make-hash))
      (hash-set*!
       contract-maps
       'GRID-SIZE
       (hash 'none any/c 'max natural?)
       'BOARD-HEIGHT
       (hash 'none any/c 'max natural?)
       'BOARD-HEIGHT-PIXELS
       (hash
        'none
        any/c
        'max
        (-> (and/c natural? (=/c (* GRID-SIZE BOARD-HEIGHT))))))))
   (define GRID-SIZE #f)
   (define BOARD-HEIGHT 1)
   (define (BOARD-HEIGHT-PIXELS) (* GRID-SIZE BOARD-HEIGHT))

```
  
  
When the test2 module gets evaluated above, no error occurs because the contract-maps submodule does not actually get evaluated as part of this. However, ****require/configurable-contract**** triggers a requirement of the contract-maps submodule:  
  
File depends-on2.rkt:  
```
#lang racket
(define-syntax ctc-level 'none)
(require "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require/configurable-contract "test2.rkt")

```
  
++→  [Macro transformation]++  
  
```
…
(begin
     (module middleman-test2.rkt racket/base
       (require racket/contract
                "test2.rkt"
                (submod "test2.rkt" contract-maps))
       (provide (contract-out)))
     (require 'middleman-test2.rkt))

```
  
  
…which then causes the error to appear:  
  
```
test2.rkt:11:42: *: contract violation
  expected: number?
  given: #f

```
  
  
The important thing here is that the contract level in both files is ****none****. The code that throws the error is from the construction of a ****max****-level contract. So we are getting a contract error from a contract that “doesn’t exist”.  
  
On the other hand, with ****define/contract … configurable-ctc****, a ****max****-level contract simply won’t appear anywhere in the text when the syntax level of its enclosing file is set to ****none****. So this problem is completely circumvented. This is why the mutation score of snake’s all-****none**** configuration went down (from 0.198 to 0.188) when we switched from prov/con to def/con. We were no longer able to catch a small set of mutants via phantom contracts.  
  
This episode highlights an important point about prov/con vs def/con: the meaning of a file’s ctc-level has entirely flipped. With prov/con, setting a file’s ctc-level to max meant “attach max contracts to everything that originates from ****elsewhere****”. With def/con, setting a file’s ctc-level to max means “attach max contracts to everything that originates from ****this file****”.  
  
