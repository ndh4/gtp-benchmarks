#lang racket

(require "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(define/ctc-helper (html-element/c name) (cons/c name list?))

(define/ctc-helper tr? (html-element/c 'tr))

(define/ctc-helper th? (html-element/c 'th))

(define/ctc-helper td? (html-element/c 'td))

(define/ctc-helper attribute? (list/c symbol? string?))

(define/ctc-helper sa? (list/c (listof attribute?) string?))

(define/ctc-helper
 purpose-to-html-args/c*
 (make-dependent-contract
  (or/c #f (list/c string? string?))
  (lambda (check?)
    (min-len-string/c (if check? (add1 (string-length (second check?))) 1)))
  #:none-val
  'none))

(define/ctc-helper purpose-to-html-check?-ctc (first purpose-to-html-args/c*))

(define/ctc-helper
 purpose-to-html-comment-ctc
 (second purpose-to-html-args/c*))

(define/ctc-helper purpose-to-html-ctc-reset!? (third purpose-to-html-args/c*))

(provide account-to-html
         assemble-page
         assemble-separators
         headers
         2-aligned-cells
         transactions-to-html
         auto-or-check-to-html
         purpose-to-html
         html-row
         date-to-html
         check-mark
         make-td)

(require "date.rkt")

(require "data.rkt")

(require (only-in xml xexpr? xexpr->xml display-xml/content))

(module+
 test
 (require (submod ".."))
 (require (submod "file-io.rkt" examples))
 (require rackunit))

(define CHECKMARK "resources/check-mark.png")

(define COMPLETE "Complete transactions for ")

(define/contract
 (account-to-html a)
 (configurable-ctc (types (-> account/c xexpr?)) (max (-> account/c xexpr?)))
 (define title (~a COMPLETE (account-name a)))
 (define date (date-to-html (account-balance-date a)))
 (define b (account-balance a))
 (define history+
   (transactions->running-balance (identity (account-history a)) b -))
 (define recents+
   (transactions->running-balance (reverse (account-recents a)) b +))
 (define recent:xexpr (transactions-to-html recents+))
 (define history:xexpr (transactions-to-html history+))
 (assemble-page
  title
  date
  (amount->decimal-string b)
  recent:xexpr
  history:xexpr))

(define/contract
 (assemble-page title date b recent:xexpr history:xexpr)
 (configurable-ctc
  (types (-> string? string? string? xexpr? xexpr? xexpr?))
  (max (-> string? string? string? xexpr? xexpr? xexpr?)))
 `(html
   (head (title ,title))
   (body
    (blockquote
     (h3 ,title)
     (table
      ((border "0") (cellpadding "0") (cellspacing "0"))
      ,(headers)
      ,@recent:xexpr
      ,@(assemble-separators date b)
      ,@(reverse history:xexpr))))))

(define/contract
 (assemble-separators date b)
 (configurable-ctc
  (types (-> string? string? (list/c tr? tr?)))
  (max (-> string? string? (list/c tr? tr?))))
 (define blk '(th " "))
 `((tr
    ((bgcolor "pink"))
    ,blk
    ,@(2-aligned-cells "History:" date)
    ,blk
    ,blk
    ,blk)
   (tr
    ((bgcolor "pink"))
    ,blk
    ,@(2-aligned-cells "Balance:" b)
    ,blk
    ,blk
    ,blk)))

(define/contract
 (headers)
 (configurable-ctc (types (-> tr?)) (max (-> tr?)))
 (html-row
  '()
  'th
  #:date
  (td-center "Date" 100)
  #:check
  (td-right "Check #" 66)
  #:purpose
  (td-center "Purpose" 200)
  #:withdrawal
  (td-right "Withdrawal" 88)
  #:deposit
  (td-right "Deposit" 88)
  #:total
  (td-right "Running Total" 111)
  #:yes
  (td-center "" 44)))

(define/contract
 (2-aligned-cells word other)
 (configurable-ctc
  (types (-> string? string? (list/c th? th? th?)))
  (max (-> string? string? (list/c th? th? th?))))
 `((th ((align "left")) ,word)
   (th ((align "left")) "   ")
   (th ((align "left")) ,other)))

(define/contract
 (transactions-to-html transactions)
 (configurable-ctc
  (types (-> (listof (list/c dw/c amount/c)) (listof tr?)))
  (max (-> (listof (list/c dw/c amount/c)) (listof tr?))))
 (for/list
  ((t (in-list transactions)) (i (in-naturals)))
  (define dw (first t))
  (define comment (dw-comment dw))
  (define amount (dw-amount dw))
  (define x (amount->decimal-string (abs amount)))
  (define check?
    (or (regexp-match #px"^ (\\d+) " comment)
        (regexp-match #px"^\\*(\\d+) " comment)))
  (html-row
   `((style
      ,(string-append "background-color:" (if (odd? i) "lightblue" "white"))))
   'td
   #:date
   (td-left (date-to-html (dw-date dw)))
   #:check
   (auto-or-check-to-html check? amount)
   #:purpose
   (td-left (purpose-to-html check? comment))
   #:withdrawal
   (td-right (if (>= amount 0) "" x))
   #:deposit
   (td-right (if (> amount 0) x ""))
   #:total
   (td-right (amount->decimal-string (second t)))
   #:yes
   (td-center (if (regexp-match "^\\*" comment) check-mark "")))))

(define/contract
 (auto-or-check-to-html check? amount)
 (configurable-ctc
  (types (-> (or/c #f (list/c string? string?)) amount/c list?))
  (max
   (->
    (or/c #f (list/c string? string?))
    amount/c
    (list/c (cons/c attribute? list?) string?))))
 (td-right (if check? (second check?) (if (< amount 0) "auto" " "))))

(define/contract
 (purpose-to-html check? comment)
 (configurable-ctc
  (types (-> (or/c #f (list/c string? string?)) string? string?))
  (max
   (->i
    ((check? purpose-to-html-check?-ctc) (comment purpose-to-html-comment-ctc))
    (_ string?)
    #:post
    ()
    (and (purpose-to-html-ctc-reset!?)))))
 (define proper (substring comment 1))
 (cond
  ((not check?) proper)
  (else
   (define end-of-check# (string-length (second check?)))
   (substring proper end-of-check#))))

(define/contract
 (html-row
  a
  tag
  #:date
  d
  #:purpose
  p
  #:withdrawal
  -
  #:deposit
  +
  #:total
  t
  #:yes
  y
  #:check
  c)
 (configurable-ctc
  (types
   (->
    (listof attribute?)
    symbol?
    #:date
    sa?
    #:purpose
    sa?
    #:withdrawal
    sa?
    #:deposit
    sa?
    #:total
    sa?
    #:yes
    sa?
    #:check
    sa?
    tr?))
  (max
   (->
    (listof attribute?)
    symbol?
    #:date
    sa?
    #:purpose
    sa?
    #:withdrawal
    sa?
    #:deposit
    sa?
    #:total
    sa?
    #:yes
    sa?
    #:check
    sa?
    tr?)))
 `(tr ,a ,@(map (lambda (x) (cons tag x)) (list d c y p - + t))))

(define/contract
 (date-to-html d)
 (configurable-ctc (types (-> my-date/c string?)) (max (-> my-date/c string?)))
 (match-define
  `(,year ,month ,day)
  (map (lambda (x) (~r x #:min-width 2 #:pad-string "0")) d))
 (string-append month " " day " " year))

(define/contract
 check-mark
 (configurable-ctc
  (types (list/c 'img (listof (list/c symbol? string?))))
  (max (list/c 'img (listof (list/c symbol? string?)))))
 `(img ((src ,CHECKMARK) (width "22") (alt "yes"))))

(define ((make-td where) content (w #f))
  (if (boolean? w)
    `(((align ,(~a where))) ,content)
    `(((align ,(~a where)) (width ,(~a w))) ,content)))

(define td-left (make-td 'left))

(define td-center (make-td 'center))

(define td-right (make-td 'right))

(module+
 test
 (define recents
   (list
    (dw " 101 a check" '(2026 6 6) -50)
    (dw " one withdrawal" '(2026 6 6) -50)
    (dw "*one deposit" '(2026 6 6) 100)))
 (define with-balances (map list recents (list 0 50 100)))
 (check-equal?
  (transactions-to-html with-balances)
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
     (td ,@(td-left "06 06 2026"))
     (td ,@(td-right " "))
     (td ,@(td-center `(img ((src ,CHECKMARK) (width "22") (alt "yes")))))
     (td ,@(td-left "one deposit"))
     (td ,@(td-right ""))
     (td ,@(td-right "100.00"))
     (td ,@(td-right "100.00")))))
 (check-equal?
  (assemble-page "title" "date" "b" '() '())
  `(html
    (head (title "title"))
    (body
     (blockquote
      (h3 "title")
      (table
       ((border "0") (cellpadding "0") (cellspacing "0"))
       ,(headers)
       ,@(assemble-separators "date" "b"))))))
 (check-equal?
  (account-to-html checking)
  `(html
    (head (title ,(~a COMPLETE (account-name checking))))
    (body
     (blockquote
      (h3 ,(~a COMPLETE (account-name checking)))
      (table
       ((border "0") (cellpadding "0") (cellspacing "0"))
       ,(headers)
       ,@(assemble-separators (date-to-html (today)) "0.00")))))))

