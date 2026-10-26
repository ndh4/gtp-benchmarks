#lang racket

(require "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

;; turn an account into an HTML page

(define/ctc-helper attribute? (list/c symbol? string?))
(define/ctc-helper sa? (list/c (listof attribute?) string?))

(define/ctc-helper my-xexpr/c
  (flat-named-contract
    'my-xexpr/c
    (flat-rec-contract
     my-xexpr
     (or/c string?
           symbol?
           (cons/c symbol? (listof my-xexpr))
           (cons/c symbol?
                   (cons/c (listof attribute?)
                           (listof my-xexpr)))))
    (lambda (fuel)
      (define (random-sym)
        (get-random-element '(tr th td html head table img left align bgcolor)))
      (define (random-str)
        (get-random-element '("text" "pink" "left" "background-color:lightblue" "title")))

      (define (generate f)
        (if (<= f 0)
            (if (zero? (random 2)) (random-str) (random-sym))
            (case (random 4)
              [(0) (random-str)]
              [(1) (random-sym)]
              [(2) (cons (random-sym) (build-list (random 3) (lambda (_) (generate (sub1 f)))))]
              [(3) (cons (random-sym) (cons `((,(random-sym) ,(random-str))) (build-list (random 2) (lambda (_) (generate (sub1 f))))))])))
      (lambda ()
        (generate fuel)))))

(define/ctc-helper (html-element/c name)
  (and/c (cons/c name list?) my-xexpr/c))

(define/ctc-helper tr? (html-element/c 'tr))
(define/ctc-helper th? (html-element/c 'th))
(define/ctc-helper td? (html-element/c 'td))

(define/ctc-helper purpose-to-html-args/c*
  (make-dependent-contract (or/c #f (list/c string? string?))
                           (lambda (check?)
                             (min-len-string/c (if check? (add1 (string-length (second check?))) 1)))
                           #:none-val 'none))
(define/ctc-helper purpose-to-html-check?-ctc (first purpose-to-html-args/c*))
(define/ctc-helper purpose-to-html-comment-ctc (second purpose-to-html-args/c*))
(define/ctc-helper purpose-to-html-ctc-reset!? (third purpose-to-html-args/c*))

(provide/configurable-contract
  [account-to-html
    ([types (-> account/c my-xexpr/c)]
     [max   (-> account/c my-xexpr/c)])]

  [assemble-page
    ([types (-> string? string? string? (listof tr?) (listof tr?) my-xexpr/c)]
     [max   (-> string? string? string? (listof tr?) (listof tr?) my-xexpr/c)])]

  [assemble-separators
    ([types (-> string? string? (list/c tr? tr?))]
     [max   (-> string? string? (list/c tr? tr?))])]

  [headers
    ([types (-> tr?)]
     [max   (-> tr?)])]

  [2-aligned-cells
    ([types (-> string? string? (list/c th? th? th?))]
     [max   (-> string? string? (list/c th? th? th?))])]

  [transactions-to-html
    ([types (-> (listof (list/c dw/c amount/c)) (listof tr?))]
     [max   (-> (listof (list/c dw/c amount/c)) (listof tr?))])]

  [auto-or-check-to-html
    ([types (-> (or/c #f (list/c string? string?)) amount/c list?)]
     [max   (-> (or/c #f (list/c string? string?)) amount/c (list/c (cons/c attribute? list?) string?))])]

  [purpose-to-html
    ([types (-> (or/c #f (list/c string? string?)) string? string?)]
     [max   (->i ([check?  purpose-to-html-check?-ctc]
                  [comment purpose-to-html-comment-ctc])
                 [_ string?]
                 #:post () (and (purpose-to-html-ctc-reset!?)))])]

  [html-row
    ([types (-> (listof attribute?) symbol? #:date sa? #:purpose sa? #:withdrawal sa? #:deposit sa? #:total sa? #:yes sa? #:check sa? tr?)]
     [max   (-> (listof attribute?) symbol? #:date sa? #:purpose sa? #:withdrawal sa? #:deposit sa? #:total sa? #:yes sa? #:check sa? tr?)])]

  [date-to-html
    ([types (-> my-date/c string?)]
     [max   (-> my-date/c string?)])]

  [check-mark
    ([types (list/c 'img (listof (list/c symbol? string?)))]
     [max   (list/c 'img (listof (list/c symbol? string?)))])]

  [make-td
    ([types (-> (or/c 'left 'center 'right) (->* (any/c) (any/c) list?))]
     [max   (-> (or/c 'left 'center 'right) (->* (any/c) (any/c) (cons/c (cons/c attribute? list?) list?)))])])

;; ---------------------------------------------------------------------------------------------------
(require "date.rkt")
(require "data.rkt")

(module+ test
  (require (submod ".."))
  (require (submod "file-io.rkt" examples))
  (require rackunit))

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

(define CHECKMARK "resources/check-mark.png")
(define COMPLETE "Complete transactions for ")

#; {Account -> Xexpr}
(define (account-to-html a)

  (define title (~a COMPLETE (account-name a)))
  (define date  (date-to-html (account-balance-date a)))

  (define b (account-balance a))
  (define history+ [transactions->running-balance (identity (account-history a)) b -])
  (define recents+ [transactions->running-balance (reverse (account-recents a)) b +])
   
  (define recent:xexpr  (transactions-to-html recents+))
  (define history:xexpr (transactions-to-html history+))
  
  (assemble-page title date (amount->decimal-string b) recent:xexpr history:xexpr))

;; ---------------------------------------------------------------------------------------------------
#; {String String String [Listof TR] [Listof TR] -> Xexpr}
(define (assemble-page title date b recent:xexpr history:xexpr)
  `(html
    (head
     (title ,title))
    (body
     (blockquote 
      (h3 ,title)
      (table ([border "0"] [cellpadding "0"] [cellspacing "0"])
             ,(headers)
             ,@recent:xexpr
             ,@(assemble-separators date b)
             ,@(reverse history:xexpr))))))

;; ---------------------------------------------------------------------------------------------------
#; {String String -> [List TR TR]}
(define (assemble-separators date b)
  (define blk '(th " "))
  `((tr ((bgcolor "pink")) ,blk ,@(2-aligned-cells "History:" date) ,blk ,blk ,blk) 
    (tr ((bgcolor "pink")) ,blk ,@(2-aligned-cells "Balance:" b)    ,blk ,blk ,blk)))

;; ---------------------------------------------------------------------------------------------------
#; {-> TR}
(define (headers)
  (html-row '() 'th
            #:date       (td-center "Date"          100)
            #:check      (td-right  "Check #"        66)
            #:purpose    (td-center "Purpose"       200)
            #:withdrawal (td-right  "Withdrawal"     88) 
            #:deposit    (td-right  "Deposit"        88) 
            #:total      (td-right  "Running Total" 111)
            #:yes        (td-center  ""              44)))

;; ---------------------------------------------------------------------------------------------------
#; {String String -> [List TH TH TH]}
(define (2-aligned-cells word other)
  `[(th ((align "left")) ,word)
    (th ((align "left")) "   ")
    (th ((align "left")) ,other)])

;; ---------------------------------------------------------------------------------------------------
#; {[Listof [List DW Amount]] -> [Listof TR]}
;; render the given list of tranasctions as HTML table rows with alternating color
(define (transactions-to-html transactions)
  (for/list ((t (in-list transactions)) (i (in-naturals)))
    (define dw      (first t))
    (define comment (dw-comment dw))
    (define amount  (dw-amount dw))
    (define x       (amount->decimal-string (abs amount)))
    
    (define check? (or (regexp-match #px"^ (\\d+) " comment) (regexp-match #px"^\\*(\\d+) " comment)))
    
    (html-row `([style ,(string-append "background-color:" (if (odd? i) "lightblue" "white"))]) 'td
              #:date       (td-left (date-to-html (dw-date dw)))
              #:check      (auto-or-check-to-html check? amount)
              #:purpose    (td-left (purpose-to-html check? comment))
              #:withdrawal (td-right (if (>= amount 0) "" x))
              #:deposit    (td-right (if (> amount 0)  x ""))
              #:total      (td-right (amount->decimal-string (second t)))
              #:yes        (td-center (if (regexp-match "^\\*" comment) check-mark "")))))

;; ---------------------------------------------------------------------------------------------------
#; {(U False [List String String]) Amount -> [List [List TD-Attribute String]]}
(define (auto-or-check-to-html check? amount)
  (td-right (if check? (second check?) (if (< amount 0) "auto" " "))))

;; ---------------------------------------------------------------------------------------------------
#; {(U False [List String String]) String -> String}
(define (purpose-to-html check? comment)
  (define proper (substring comment 1))
  (cond
    [(not check?) proper]
    [else 
     (define end-of-check# (string-length (second check?)))
     (substring proper end-of-check#)]))

;; ---------------------------------------------------------------------------------------------------
;; Attributes = [Listof [List Symbol String]] 
;; SA = [List Attributes String]

#; {TR-Attributes CellTag #:date SA #:purpose SA #:withdrawal SA #:deposit SA #:total SA #:yes SA
                  -> 
                  TR}
(define (html-row a tag #:date d #:purpose p #:withdrawal - #:deposit + #:total t #:yes y #:check c)
  ;; CONTRACT only `-` or `+` can come with a non-empty string 
  `(tr ,a ,@(map (lambda (x) (cons tag x)) (list d c y p - + t))))

(define (date-to-html d)
  (match-define `(,year ,month ,day) (map (lambda (x) (~r x #:min-width 2 #:pad-string "0")) d))
  (string-append month " " day " " year))

(define check-mark `(img ((src ,CHECKMARK) (width "22") (alt "yes"))))

(define ((make-td where) content (w #false))
  (if (boolean? w)
      `(((align ,(~a where))) ,content)
      `(((align ,(~a where)) (width ,(~a w))) ,content)))

(define td-left   (make-td 'left))
(define td-center (make-td 'center))
(define td-right  (make-td 'right))

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
  (define recents
    (list
     (dw " 101 a check"    '(2026 6 6)  -50)
     (dw " one withdrawal" '(2026 6 6)  -50)
     (dw "*one deposit"    '(2026 6 6) +100)))
  (define with-balances (map list recents (list 0 50 100)))

  (check-equal? (transactions-to-html with-balances)
                `((tr
                   ((style "background-color:white"))
                   (td ,@(td-left "06 06 2026"))
                   (td ,@(td-right "101"))
                   (td ,@(td-center ""))
                   (td ,@(td-left " a check"))
                   (td ,@(td-right "50.00"))
                   (td ,@(td-right ""))
                   (td ,@(td-right "0.00")))
                  (tr
                   ((style "background-color:lightblue"))
                   (td ,@(td-left "06 06 2026"))
                   (td ,@(td-right "auto"))
                   (td ,@(td-center ""))
                   (td ,@(td-left "one withdrawal"))
                   (td ,@(td-right "50.00"))
                   (td ,@(td-right ""))
                   (td ,@(td-right "50.00")))
                  (tr
                   ((style "background-color:white"))
                   (td ,@(td-left  "06 06 2026"))
                   (td ,@(td-right " "))
                   (td ,@(td-center `(img ((src ,CHECKMARK) (width "22") (alt "yes")))))
                   (td ,@(td-left "one deposit"))
                   (td ,@(td-right ""))
                   (td ,@(td-right "100.00"))
                   (td ,@(td-right "100.00")))))
  
  (check-equal? (assemble-page "title" "date" "b" '[] '[])
                `(html
                  (head (title "title"))
                  (body
                   (blockquote
                    (h3 "title")
                    (table
                     ((border "0") (cellpadding "0") (cellspacing "0"))
                     ,(headers)
                     ,@(assemble-separators "date" "b"))))))
  
  (check-equal? (account-to-html checking)
                `(html
                  (head (title ,(~a COMPLETE (account-name checking))))
                  (body
                   (blockquote
                    (h3 ,(~a COMPLETE (account-name checking)))
                    (table
                     ((border "0") (cellpadding "0") (cellspacing "0"))
                     ,(headers)
                     ,@(assemble-separators (date-to-html (today)) "0.00")))))))