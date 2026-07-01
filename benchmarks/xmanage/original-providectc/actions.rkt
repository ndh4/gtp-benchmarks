#lang racket

;; the actions that can be performed on an account

;; the result plus action setup makes unit testing convenient and effective
;; but is useless for the actual script. 
(define (action/c x/c)
  (list/c x/c (-> any)))

(provide
 (rename-out [to-html to-html/t])

 (contract-out
  [name->path
   (->* (string?) (#:kind string?) path?)]

  [write 
   (-> account? (-> void?))]

  [update
   (-> account? my-date? account?)]

  [new-account/2
   (-> procedure? string? (action/c account?))]
  [deposit
   ;; consumes command-line arguments to deposit amount 
   (->* (account? my-date? string?) () #:rest (listof string?) (action/c account?))]
  [withdraw
   ;; consumes commad-line arguments to withdraw amount
   (->* (account? my-date? string?) () #:rest (listof string?) (action/c account?))]
  [write-check
   ;; consumes commad-line arguments to record a check at amount
   (->* (account? my-date? string?) () #:rest (listof string?) (action/c account?))]
  [show-balance
   (->* (account? my-date?) () #:rest any/c (action/c amount?))]
  [to-html
   (->* (account? my-date?) () #:rest any/c (action/c any/c))]))

;; ---------------------------------------------------------------------------------------------------
(require "date.rkt")
(require "data.rkt")
(require "file-io.rkt")
(require "html.rkt")
(require racket/control)
(require (only-in xml xexpr->xml display-xml/content))

(module+ test
  (require (submod ".."))
  (require (submod "file-io.rkt" examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;                                                   
;                                                   
;            ;                                      
;            ;                                      
;    ;;;   ;;;;;   ;;;    ;;;;  ;;;;    ;;;;   ;;;  
;   ;   ;    ;    ;; ;;   ;;  ;     ;  ;;  ;  ;;  ; 
;   ;        ;    ;   ;   ;         ;  ;   ;  ;   ;;
;    ;;;     ;    ;   ;   ;      ;;;;  ;   ;  ;;;;;;
;       ;    ;    ;   ;   ;     ;   ;  ;   ;  ;     
;   ;   ;    ;    ;; ;;   ;     ;   ;  ;; ;;  ;     
;    ;;;     ;;;   ;;;    ;      ;;;;   ;;;;   ;;;; 
;                                          ;        
;                                       ;  ;        
;                                        ;;         

(define HOME (find-system-path 'home-dir))

(define (name->path name #:kind (proper-or-html #false))
  (define file-name (if proper-or-html (~a name "." proper-or-html) (~a "." name ".act")))
  (build-path HOME "Private" "Accounts" "Xmanage" file-name))

(define [(write account)]
  (define file-name (name->path (account-name account)))
  (with-output-to-file file-name #:exists 'replace (λ () (account-writer account))))

(module+ test
  (check-equal? (name->path "name") (build-path HOME "Private" "Accounts" "Xmanage" ".name.act"))
  (check-equal? (name->path "n" #:kind "h") (build-path HOME "Private" "Accounts" "Xmanage" "n.h"))
  
  (let ([file-name (name->path (account-name checking))])
    (dynamic-wind
     void
     (λ () (check-true (void? [(write checking)]) "dumb check"))
     (λ () (delete-file file-name)))))


;                       
;                       
;                       
;                       
;   ; ;;    ;;;  ;     ;
;   ;;  ;  ;;  ; ;     ;
;   ;   ;  ;   ;; ; ; ; 
;   ;   ;  ;;;;;; ; ; ; 
;   ;   ;  ;      ;; ;; 
;   ;   ;  ;      ;; ;; 
;   ;   ;   ;;;;   ; ;  
;                       
;                       
;                       

(define ALREADY-EXISTS "An account with this name already exists.\n")

#; {(Natural -> α)  String -> Void}
;; the α is Tim Griffin's continuation trick (argh)
(define (new-account/2 exit-for-testing name #:last-check (last 0))
  (define a (account name '() 0 (today) '() last))
  (define (effect)
    (define file-name (name->path name))
    (when (file-exists? file-name)
      (printf ALREADY-EXISTS)
      (exit-for-testing 1))
    (with-output-to-file file-name (λ () (account-writer a))))
  (list a effect))

;                                                                                                    
;              ;      ;                                     ;                                        
;              ;      ;           ;;;                       ;        ;                           ;   
;              ;      ;          ;                          ;        ;                           ;   
;   ;;;;    ;;;;   ;;;;          ;             ;;;   ;   ;  ;;;;   ;;;;;   ;;;;  ;;;;    ;;;   ;;;;; 
;       ;  ;; ;;  ;; ;;          ;;           ;   ;  ;   ;  ;; ;;    ;     ;;  ;     ;  ;;  ;    ;   
;       ;  ;   ;  ;   ;          ;;           ;      ;   ;  ;   ;    ;     ;         ;  ;        ;   
;    ;;;;  ;   ;  ;   ;         ;  ; ;         ;;;   ;   ;  ;   ;    ;     ;      ;;;;  ;        ;   
;   ;   ;  ;   ;  ;   ;         ;  ;;;            ;  ;   ;  ;   ;    ;     ;     ;   ;  ;        ;   
;   ;   ;  ;; ;;  ;; ;;         ;;  ;         ;   ;  ;   ;  ;; ;;    ;     ;     ;   ;  ;;       ;   
;    ;;;;   ;;;;   ;;;;          ;;; ;         ;;;    ;;;;  ;;;;     ;;;   ;      ;;;;   ;;;;    ;;; 
;                                                                                                    
;                                                                                                    
;                                                                                                    

#; {{U + -} -> (Account Date Amount Any ... -> Account)}
(define ((make-d/w how) account date  a . msg)
  (define amount
    (match a
      [(argv-amount> b) b]
      [_
       (error 'xmanage "amount expected, found ~a" a)]))
  (define history (account-recents account))
  (define delta   (how 0 amount))
  (define action  (dw (align (string-join msg)) date delta))
  (set-account-recents! account (cons action history))
  (list account (write account)))

#; (Account Date Amount String -> Void)
;; effect: record a deposit
(define deposit (make-d/w +))

#; (Account Date Amount String -> Void)
;; effect: record a non-check withdrawl
(define withdraw (make-d/w -))

#; (Account Date Amount String ... -> Void)
;; EFFECT confirm check writing action with a print to console 
(define (write-check account date amount . purpose)
  (match-define (list the-check bump) (create-check account (string-join purpose)))
  (match-define (list account+  do) (withdraw account date amount the-check))
  (define (ackn) (printf "Check No. ~a Today: ~a~n" (account-check-no account) (today)))
  ;; increment check# only when ready to write a successful subtraction
  (list account+ (λ () (bump) (do) (ackn))))

#; [Account String -> String]
;; create entry for a check in `acc` 
;; EFFECT increase check number in account 
(define (create-check acc purpose)
  (define x (+ (account-check-no acc) 1))
  (define !
    (if (equal? (string-trim purpose) "")
        (λ () (eprintf "warning: the purpose statement is missing\n"))
        void))
  (list (~a x " " purpose) (lambda () (!) (set-account-check-no! acc x))))

;; ---------------------------------------------------------------------------------------------------

;                                                   
;   ;                                               
;   ;             ;;;                               
;   ;               ;                               
;   ;;;;   ;;;;     ;    ;;;;   ; ;;    ;;;    ;;;  
;   ;; ;;      ;    ;        ;  ;;  ;  ;;  ;  ;;  ; 
;   ;   ;      ;    ;        ;  ;   ;  ;      ;   ;;
;   ;   ;   ;;;;    ;     ;;;;  ;   ;  ;      ;;;;;;
;   ;   ;  ;   ;    ;    ;   ;  ;   ;  ;      ;     
;   ;; ;;  ;   ;    ;    ;   ;  ;   ;  ;;     ;     
;   ;;;;    ;;;;     ;;   ;;;;  ;   ;   ;;;;   ;;;; 
;                                                   
;                                                   
;                                                   

(define (show-balance a _date . _others)
  (define b (+ (transactions->balance (account-recents a)) (account-balance a)))
  (list b (λ () (printf "Current balance: ~a\n" (amount->decimal-string b)))))

;                                            
;                     ;                      
;                     ;           ;          
;                     ;           ;          
;   ;   ;  ;;;;    ;;;;  ;;;;   ;;;;;   ;;;  
;   ;   ;  ;; ;;  ;; ;;      ;    ;    ;;  ; 
;   ;   ;  ;   ;  ;   ;      ;    ;    ;   ;;
;   ;   ;  ;   ;  ;   ;   ;;;;    ;    ;;;;;;
;   ;   ;  ;   ;  ;   ;  ;   ;    ;    ;     
;   ;   ;  ;; ;;  ;; ;;  ;   ;    ;    ;     
;    ;;;;  ;;;;    ;;;;   ;;;;    ;;;   ;;;; 
;          ;                                 
;          ;                                 
;          ;                                 

(define (update account action-date)
  (define last-date (account-balance-date account))
  (when (next-month last-date action-date)
    (define-values (balance history)
      (for/fold ([balance (account-balance account)]
                 [history (account-history account)])
                ([r (account-recents account)])
        (values (+ (dw-amount r) balance)
                (cons r history))))
    (set-account-recents! account '())
    (set-account-history! account history)
    (set-account-balance-date! account action-date)
    (set-account-balance! account balance))
  account)

(module+ test
  (let () ;; test update 
    (define Acopied  (struct-copy account A+100chk))
    (define nextdate '(2026 8 20))
    (define exp (account "checking" '() 100  nextdate (account-recents A+100chk) 0))
    (check-equal? (update Acopied nextdate) exp "update: check return")
    (check-equal? Acopied exp "update: check effect")))


  
;                                            
;                            ;               
;                            ;               
;                            ;               
;    ;;;;   ;;;   ; ;;    ;;;;   ;;;    ;;;; 
;    ;;  ; ;;  ;  ;;  ;  ;; ;;  ;;  ;   ;;  ;
;    ;     ;   ;; ;   ;  ;   ;  ;   ;;  ;    
;    ;     ;;;;;; ;   ;  ;   ;  ;;;;;;  ;    
;    ;     ;      ;   ;  ;   ;  ;       ;    
;    ;     ;      ;   ;  ;; ;;  ;       ;    
;    ;      ;;;;  ;   ;   ;;;;   ;;;;   ;    
;                                            
;                                            
;                                            

#; {Account Date -> Void}
(define (to-html a my-dste #:open (open "open") . _other)
  (define page:xml (xexpr->xml (account-to-html a)))
  (define file     (name->path (account-name a) #:kind 'html))
  (list '_
        (λ ()
          (with-output-to-file file #:exists 'replace (lambda () (display-xml/content page:xml)))
          (system (~a open " " (path->string file)))
          (sleep 1))))

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

(module+ test 
  #; {String -> Account}
  ;; EFFECT run the effect of making a new account 
  (define (run-new-account/2 name)
    (prompt 
     (match-define (list a do) (new-account/2 (λ (x) (control k x)) name))
     (do)
     a))

  #; {Procedure Any ... -> Any}
  (define (run f . others)
    (match-define (list r _) (apply f others))
    r)

  #; {(Account #:file-to-be-created Any ... -> [List Any (-> Void)])}
  (define (run-for-effect f  a #:file-to-be-created (fn (name->path (account-name a))) . others)
    (match-define (list _ do) (apply f a others))
    (dynamic-wind
     (λ ()
       (delete-directory/files fn #:must-exist? #false))
     (λ ()
       (begin0
         (with-output-to-string do)
         (check-true (file-exists? fn))))
     (λ ()
       (sleep .1)
       (delete-file fn)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; new account
  (define TTT "ttt")

  (dynamic-wind
   (λ ()
     (define a (run-new-account/2 TTT))
     (check-equal? (run show-balance a (today)) 0)
     (check-equal? (with-output-to-string (λ () [(second (show-balance a (today)))]))
                   "Current balance: 0.00\n"))

   (λ ()
     ;; re-create to trigger failure
     (check-match (with-output-to-string (λ () (check-equal? (run-new-account/2 TTT) 1)))
                  (pregexp ALREADY-EXISTS)))
   
   (λ ()
     (delete-file (name->path TTT)))))
;; ---------------------------------------------------------------------------------------------------
(module+ test ;; testing deposits, withdrawals, and check writing
  (define 2day '(2026 6 6))

  (check-equal? (run deposit (struct-copy account Achk) 2day "100" 100purpose) A+100chk)
  (check-equal? (run withdraw (struct-copy account A+100chk) 2day "50" 50purpose) A+100-50chk)
  (let ([a (struct-copy account A+100+25chk)])
    ;; the effect is delayed until `main` allows it
    (set-account-check-no! a 0)
    (check-equal? (run write-check (struct-copy account A+100chk) 2day "25" 25purpose) a)))
                

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; check that it creates the account file and prints confirmation message to STDOUT 
  (check-match (run-for-effect write-check (struct-copy account A+100chk) 2day "25" 25purpose)
               (pregexp "Check No.")))
;; ---------------------------------------------------------------------------------------------------
(module+ test ;; check that it creates the HTML file 
  (let* ((a (struct-copy account A+100chk))
         (f (name->path (account-name a) #:kind "html")))
    (check-equal? (run-for-effect (λ x (apply to-html/t #:open "ls" x)) a 2day #:file-to-be-created f)
                  (~a (path->string f) "\n")))

  (let ([a (struct-copy account A+100chk)])
    (check-exn #px"amount expected" (λ () (run write-check a 2day "-1.00" 25purpose)))
    (check-equal? a A+100chk "the account remains unmodified due to bad amount")))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; error checking
  (check-exn #px"amount expected"
             (λ () (run deposit (struct-copy account Achk) 2day "1h" "6" 100purpose))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; tests for balance
  (check-equal? (run show-balance Achk 2day) 0)
  (check-equal? (run show-balance A+100chk 2day) 100)
  (check-equal? (run show-balance A+100+25chk 2day) 75))
