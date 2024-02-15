#lang racket

;; ===================================================================================================

(require
 (only-in "t-view.rkt" manage-c/types-ctc manage-c/max-ctc)
 "../../../ctcs/precision-config.rkt"
 "../../../ctcs/common.rkt"
 "../../../ctcs/configurable.rkt"
 "helpers.rkt"
 ;; "t-graph.rkt"
 )
(require/configurable-contract "t-graph.rkt" mbta% read-t-graph )
(require/configurable-contract "t-view.rkt" manage% SWITCH ENSURE ENABLED-0 DISABLED-0 ENABLED DISABLED NO-PATH DESTINATION-0 DESTINATION CURRENT-LOCATION-0 CURRENT-LOCATION INTERNAL selector )

(provide/configurable-contract
 [PATH ([max (λ (re)
          (equal? #rx"from (.*) to (.*)$" re))]
   [types regexp?])]
 [DISABLE ([max (λ (re)
          (equal? #rx"disable (.*)$" re))]
   [types regexp?])]
 [ENABLE ([max (λ (re)
          (equal? #rx"enable (.*)$" re))]
   [types regexp?])]
 [DONE ([max "done"]
   [types string?])]
 [EOM ([max "eom"]
   [types string?])]
 [manage ([max (instanceof/c manage-c/max-ctc)]
   #;[max/sub1 (instanceof/c manage-c/max/sub1-ctc)]
   [types (instanceof/c manage-c/types-ctc)])]
 [run-t ([max (->i ([next string?])
                   #:pre (next)
                   (begin
                     (when (not (regexp-match PATH next))
                       (set-box! stash-len (length (get-field disabled manage))))
                     (cond
                       [(regexp-match PATH next) => (lambda (x) (and (second x) (third x)))]
                       [(regexp-match DISABLE next) => second]
                       [(regexp-match ENABLE next) => second]
                       [else "message not understood"]))

                   [result (next)
                           (λ (res)
                             (cond
                               [(regexp-match PATH next)
                                => (lambda (x)
                                     (let ([x2 (second x)]
                                           [x3 (third x)])
                                       ;; this really shouldn't be being checked here, it's describing the behavior of mbta%...
                                       (if (substring? "\n" res)
                                           (and (substring? x2 res)
                                                (substring? x3 res))
                                           (or (substring? x2 res)
                                               (substring? x3 res)))))]
                               [(or (regexp-match DISABLE next)
                                    (regexp-match ENABLE next))
                                => (lambda (x)
                                     (or (equal? res "done")
                                         (string-contains? res (second x))))]
                               [else "message not understood"]))]
                   #:post (next)
                   ;; similar comment to above. Here I suppose this is a proxy for "did the appropriate method of `manage` actually get called".
                   (cond
                     [(regexp-match DISABLE next)
                      (let ([x2 (second (regexp-match DISABLE next))])
                        (> (length (get-field disabled manage))
                           (unbox stash-len)))]
                     [(regexp-match ENABLE next)
                      (<= (length (get-field disabled manage))
                          (unbox stash-len))]
                     [else #t]))]
   [types (-> string? string?)])])

;; (provide
;;  ;; String 
;;  EOM
;;  DONE
 
;;  ;; constants, regexps that match PATH, DISABLE, and ENABLE requests
;;  PATH 
;;  DISABLE
;;  ENABLE
 
;;  ;; InputPort OutputPort -> Void 
;;  ;; read FROM, DISABLE, and ENABLE requests input-port, write responses to output-port, loop
;;  run-t)


(define PATH
  #rx"from (.*) to (.*)$")

(define DISABLE
  #rx"disable (.*)$")

(define ENABLE
  #rx"enable (.*)$")

(define DONE
  "done")

(define EOM
  "eom")

(define manage
  (new manage%))

(define/ctc-helper stash-len (box #f))
(define (run-t next)
  (cond
      [(regexp-match PATH next)
       => (lambda (x)
       (define x2 (second x))
       (define x3 (third x))
       (unless (and x2 x3) (error 'run-t "invariat error"))
       (send manage find x2 x3))]
      [(regexp-match DISABLE next)
       => (lambda (x)
       (define x2 (second x))
       (unless x2 (error 'run-t "invariants"))
       (status-check add-to-disabled x2))]
      [(regexp-match ENABLE next)
       => (lambda (x)
       (define x2 (second x))
       (unless x2 (error 'run-t "invariants"))
       (status-check remove-from-disabled x2))]
      [else "message not understood"]))

(define-syntax-rule
  (status-check remove-from-disabled enabled)
  (let ([status (send manage remove-from-disabled enabled)])
    (if (boolean? status)
        DONE
        status)))
