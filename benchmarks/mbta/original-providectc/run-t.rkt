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
                     ;; nh: Both arms should use <= or >= because the station might be nonsense
                     ;; ("ENABLE blahblahblah") in which case the number of disabled stations
                     ;; does not actually change.
                     [(regexp-match DISABLE next)
                      (let ([x2 (second (regexp-match DISABLE next))])
                        (>= (length (get-field disabled manage))
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

(module+ test
  (require rackunit)
  (define (path-len p) (length (string-split p "\n")))
  ;; tests from main
  (check-equal? (path-len (run-t "from Airport to Northeastern")) 14)
  (define r1 (run-t "disable Government"))
  (check-equal? (path-len r1) 1)
  (check-equal? (path-len (run-t "from Airport to Northeastern")) 16)
  (define r2 (run-t "enable Government"))
  (check-equal? (path-len r2) 1)
  (check-equal? (path-len (run-t "from Airport to Harvard Square")) 12)
  (define r3 (run-t "disable Park Street"))
  (check-equal? (path-len r3) 1)
  (check-true (string-prefix? (run-t "from Northeastern to Harvard Square") "it is currently impossible"))
  (define r4 (run-t "enable Park Street"))
  (check-equal? (path-len r4) 1)
  (check-equal? (path-len (run-t "from Northeastern to Harvard Square")) 12)

  ;; a couple other tests
  ;; incorrectly formatted message
  (check-equal? (run-t "blabla") "message not understood")
  ;; space in station name 
  (check-equal? (run-t "from abcd abcd to abcdeee") "no such station: abcd abcd")
  ;; station that doesn't exist to station that does
  (check-equal? (run-t "from blabla to Northeastern") "no such station: blabla")
  ;; many matches (left)
  (check-true (string-prefix? (run-t "from N to Northeastern") "disambiguate your current location"))
  ;; many matches (right)
  (check-true (string-prefix? (run-t "from Northeastern to N") "disambiguate your destination"))
  ;; many matches (both)
  (check-true (string-prefix? (run-t "from N to G") "disambiguate your current location"))
  ;; many matches (both, same)
  (check-true (string-prefix? (run-t "from N to N") "disambiguate your current location"))
  ;; disabling station that doesn't exist
  (check-equal? (run-t "disable blabla") "no such station to disable: blabla")
  ;; station to itself
  (check-true (string-prefix? (run-t "from Northeastern to Northeastern") "Close your eyes"))
  ;; station to itself with different strings used to match (this one caught a bug!)
  (check-true (string-prefix? (run-t "from Northeaster to Northeastern") "Close your eyes"))
  )
