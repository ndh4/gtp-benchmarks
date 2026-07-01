#lang racket

;; i/o format and functions for reading/writing accounts

(define balanced-account/c
  (and account?
       (flat-named-contract
        "current balance equals sum of transactions"
        (λ (a)
          (= (transactions->balance (account-history a))
             (account-balance a))))))

(provide
 (contract-out
  [account-reader
   (-> balanced-account/c)]
  
  [account-writer
   (-> balanced-account/c any)]))

(module+ examples
  (provide (all-defined-out)))

;; -------------------------------------------------------------------------------------------------
(require "date.rkt")
(require "data.rkt")

(module+ examples
  (require (submod "data.rkt" examples))
  (provide (all-defined-out)))

(module+ test
  (require (submod ".." examples))
  (require (submod ".."))
  (require rackunit))

;                                            
;      ;;                                    
;     ;                                  ;   
;     ;                                  ;   
;   ;;;;;   ;;;    ;;;; ;;;;;;  ;;;;   ;;;;; 
;     ;    ;; ;;   ;;  ;;  ;  ;     ;    ;   
;     ;    ;   ;   ;    ;  ;  ;     ;    ;   
;     ;    ;   ;   ;    ;  ;  ;  ;;;;    ;   
;     ;    ;   ;   ;    ;  ;  ; ;   ;    ;   
;     ;    ;; ;;   ;    ;  ;  ; ;   ;    ;   
;     ;     ;;;    ;    ;  ;  ;  ;;;;    ;;; 
;                                            
;                                            
;                                            

#; {external A      = [String [DWList ...] Amount Date [DWList ...]] Natural}
#; {external DWList = [String Date (U AmountAsString Amount)]}

(module+ examples
  (define 100purpose "one deposit")
  (define 50purpose  "one withdrawal")
  (define 25purpose  "one check written")  

  (define (one-recent-plus-100 A (d "100.00") (msg 100purpose))
    (match A
      [(list name recents balance date history)
       (define dw (list (align msg) '(2026 6 6) d))
       (list name (cons dw recents) balance date history)]))

  (define (one-recent-minus-100 A)
    (match A
      [(list name recents balance date history)
       (define dw (list (align 50purpose) '(2026 6 6) "-50.00"))
       (list name (cons dw recents) balance date history)]))
  
  (define (Achecking0 (x 0))
    (account->A (checking0 x)))
  
  (define checking     [checking0])
  
  (define A            (account->A checking))
  (define Achk         (A->account A 0))
  
  (define A+100        (one-recent-plus-100 (Achecking0)))
  (define A+100chk     (A->account A+100 0))

  (define 1-25purpose  (~a (add1 (account-check-no A+100chk)) " " 25purpose
                           ;; the next line cheats: 30 comes from actions.rkt
                           #:max-width 30 #:min-width 30 #:left-pad-string " "))
  (define A+100+25    (one-recent-plus-100 (one-recent-plus-100 (Achecking0 1)) "-25.00" 1-25purpose))
  (define A+100+25chk  (A->account A+100+25 1))
  
  (define A+100-50     (one-recent-minus-100 (one-recent-plus-100 (Achecking0))))
  (define A+100-50chk  (A->account A+100-50 0))

  (define chase-chk
    (with-input-from-file "../.sample.act" (λ () (account-reader)))))

;                                     
;                                     
;      ;                          ;   
;                                 ;   
;    ;;;   ; ;;   ;;;;   ;   ;  ;;;;; 
;      ;   ;;  ;  ;; ;;  ;   ;    ;   
;      ;   ;   ;  ;   ;  ;   ;    ;   
;      ;   ;   ;  ;   ;  ;   ;    ;   
;      ;   ;   ;  ;   ;  ;   ;    ;   
;      ;   ;   ;  ;; ;;  ;   ;    ;   
;    ;;;;; ;   ;  ;;;;    ;;;;    ;;; 
;                 ;                   
;                 ;                   
;                 ;                   

#; {-> Account}
;; EFFECT read an A-expression from port
;; EFFECT may raise exceptions due to ill-formed file content
(define (account-reader)
  (A->account (read) (read)))
 
#; {S-expression S-expression -> Account}
(define (A->account x last-check#)
  (parameterize ([read-decimal-as-inexact #false])
    (match x
      [(list (? string? name)
             (app list*->dw recents)
             (amount> balance)
             (? my-date? date)
             (app list*->dw history))
       (define expected-balance (transactions->balance history))
       (unless (= expected-balance balance)
         (error 'account-reader "~a history does not add up to balance, found ~a vs ~a (delta = ~a)"
                name
                (exact->inexact balance)
                (exact->inexact expected-balance)
                (exact->inexact (- balance expected-balance))))
       (account name recents  balance date history last-check#)]
      [_ (error 'A->account "A expression expected, found ~a ~a" x last-check#)])))

#; {S-expression -> [Listof DW]}
(define (list*->dw x)
  (unless (list? x)
    (error 'list*->dw "[listof DWList] expected, found ~a" x))
  (map list->dw x))

#; {(List String Date String) -> DW}
(define (list->dw xy)
  (match xy
    [(list (? string? comment) (? my-date? date) (? amount? amount))
     (dw comment date amount)]
    [(list (? string? comment) (? my-date? date) (amount> amount))
     (dw comment date amount)]
    [_
     (error 'list->dw "DWList expected, found ~a" xy)]))

(module+ test
  (check-equal? (list->dw (list "a" '(2026 6 6) -100)) (dw "a" '(2026 6 6) -100) "coverage 1"))

;                                            
;                                            
;                   ;                    ;   
;                   ;                    ;   
;    ;;;   ;   ;  ;;;;;  ;;;;   ;   ;  ;;;;; 
;   ;; ;;  ;   ;    ;    ;; ;;  ;   ;    ;   
;   ;   ;  ;   ;    ;    ;   ;  ;   ;    ;   
;   ;   ;  ;   ;    ;    ;   ;  ;   ;    ;   
;   ;   ;  ;   ;    ;    ;   ;  ;   ;    ;   
;   ;; ;;  ;   ;    ;    ;; ;;  ;   ;    ;   
;    ;;;    ;;;;    ;;;  ;;;;    ;;;;    ;;; 
;                        ;                   
;                        ;                   
;                        ;                   

#; {Account -> Void}
;; EFFECT output account as an A-expression
(define (account-writer x)
  (pretty-write (account->A x))
  (display (account-check-no x))
  (display "; last check written"))

#; {Account -> A}
(define (account->A x)
  (list (account-name x)
        (map dw->list (account-recents x))
        (amount->decimal-string (account-balance x))
        (account-balance-date x)
        (map dw->list (account-history x))))

#; {DW -> [List String Date String]}
(define (dw->list dw)
  (list (dw-comment dw) (dw-date dw) (amount->decimal-string (dw-amount dw))))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test ;; basic round-trip format tests 
  (check-equal? (A->account A (account-check-no checking)) checking "new")
  (check-equal? (account->A (A->account A+100 0)) A+100 "plus")
  (check-equal? (account->A (A->account A+100-50 0)) A+100-50 "plus"))

(module+ test ;; error cases
  (check-exn #px"A expression expected" (λ () (A->account "hell" 0)))
  (check-exn #px"list" (λ () (A->account '("checking" "hey" 100 '(2026 6 6) '()) 0)))
  (check-exn #px"DWList" (λ () (A->account '("checking" '("hey") 100 '(2026 6 6) '()) 0))))

(module+ test ;; basic round-trip I/O tests
  (define A+100-50-file
    (with-output-to-string
      (λ ()
        (define a (A->account A+100-50 0))
        (account-writer a))))
  (check-equal? (with-input-from-string A+100-50-file account-reader)
                (A->account A+100-50 0)
                "basic I/O back and forth"))

(module+ test
  #;
  (check-exn #px"does not add up" (λ () (with-input-from-file "../../.check.act" account-reader)))

  #;
  (check-true (account? (with-input-from-file "../.van.act" account-reader))
              "check consistency of van account for validity")

  (check-true (account? (with-input-from-file "../.sample.act" account-reader))
              "check consistency of chase account for validity"))
