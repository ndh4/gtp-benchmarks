#lang racket

(require (only-in "t-view.rkt" manage-c/types-ctc manage-c/max-ctc)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         "helpers.rkt")

(require (only-in "t-graph.rkt" mbta% read-t-graph))

(require (only-in
          "t-view.rkt"
          manage%
          SWITCH
          ENSURE
          ENABLED-0
          DISABLED-0
          ENABLED
          DISABLED
          NO-PATH
          DESTINATION-0
          DESTINATION
          CURRENT-LOCATION-0
          CURRENT-LOCATION
          INTERNAL
          selector))

(provide PATH DISABLE ENABLE DONE EOM manage run-t)

(define/contract
 PATH
 (configurable-ctc
  (max (λ (re) (equal? #rx"from (.*) to (.*)$" re)))
  (types regexp?))
 #rx"from (.*) to (.*)$")

(define/contract
 DISABLE
 (configurable-ctc
  (max (λ (re) (equal? #rx"disable (.*)$" re)))
  (types regexp?))
 #rx"disable (.*)$")

(define/contract
 ENABLE
 (configurable-ctc
  (max (λ (re) (equal? #rx"enable (.*)$" re)))
  (types regexp?))
 #rx"enable (.*)$")

(define/contract DONE (configurable-ctc (max "done") (types string?)) "done")

(define/contract EOM (configurable-ctc (max "eom") (types string?)) "eom")

(define/contract
 manage
 (configurable-ctc
  (max (instanceof/c manage-c/max-ctc))
  (types (instanceof/c manage-c/types-ctc)))
 (new manage%))

(define/ctc-helper stash-len (box #f))

(define/contract
 (run-t next)
 (configurable-ctc
  (max
   (->i
    ((next string?))
    #:pre
    (next)
    (begin
      (when (not (regexp-match PATH next))
        (set-box! stash-len (length (get-field disabled manage))))
      (cond
       ((regexp-match PATH next) => (lambda (x) (and (second x) (third x))))
       ((regexp-match DISABLE next) => second)
       ((regexp-match ENABLE next) => second)
       (else "message not understood")))
    (result
     (next)
     (λ (res)
       (cond
        ((regexp-match PATH next)
         =>
         (lambda (x)
           (let ((x2 (second x)) (x3 (third x)))
             (if (substring? "\n" res)
               (and (substring? x2 res) (substring? x3 res))
               (or (substring? x2 res) (substring? x3 res))))))
        ((or (regexp-match DISABLE next) (regexp-match ENABLE next))
         =>
         (lambda (x)
           (or (equal? res "done") (string-contains? res (second x)))))
        (else "message not understood"))))
    #:post
    (next)
    (cond
     ((regexp-match DISABLE next)
      (let ((x2 (second (regexp-match DISABLE next))))
        (>= (length (get-field disabled manage)) (unbox stash-len))))
     ((regexp-match ENABLE next)
      (<= (length (get-field disabled manage)) (unbox stash-len)))
     (else #t))))
  (types (-> string? string?)))
 (cond
  ((regexp-match PATH next)
   =>
   (lambda (x)
     (define x2 (second x))
     (define x3 (third x))
     (unless (and x2 x3) (error 'run-t "invariat error"))
     (send manage find x2 x3)))
  ((regexp-match DISABLE next)
   =>
   (lambda (x)
     (define x2 (second x))
     (unless x2 (error 'run-t "invariants"))
     (status-check add-to-disabled x2)))
  ((regexp-match ENABLE next)
   =>
   (lambda (x)
     (define x2 (second x))
     (unless x2 (error 'run-t "invariants"))
     (status-check remove-from-disabled x2)))
  (else "message not understood")))

(define-syntax-rule
 (status-check remove-from-disabled enabled)
 (let ((status (send manage remove-from-disabled enabled)))
   (if (boolean? status) DONE status)))

#;(module+
 test
 (require rackunit)
 (define (path-len p) (length (string-split p "\n")))
 (check-equal? (path-len (run-t "from Airport to Northeastern")) 14)
 (define r1 (run-t "disable Government"))
 (check-equal? (path-len r1) 1)
 (check-equal? (path-len (run-t "from Airport to Northeastern")) 16)
 (define r2 (run-t "enable Government"))
 (check-equal? (path-len r2) 1)
 (check-equal? (path-len (run-t "from Airport to Harvard Square")) 12)
 (define r3 (run-t "disable Park Street"))
 (check-equal? (path-len r3) 1)
 (check-true
  (string-prefix?
   (run-t "from Northeastern to Harvard Square")
   "it is currently impossible"))
 (define r4 (run-t "enable Park Street"))
 (check-equal? (path-len r4) 1)
 (check-equal? (path-len (run-t "from Northeastern to Harvard Square")) 12)
 (check-equal? (run-t "blabla") "message not understood")
 (check-equal?
  (run-t "from abcd abcd to abcdeee")
  "no such station: abcd abcd")
 (check-equal? (run-t "from blabla to Northeastern") "no such station: blabla")
 (check-true
  (string-prefix?
   (run-t "from N to Northeastern")
   "disambiguate your current location"))
 (check-true
  (string-prefix?
   (run-t "from Northeastern to N")
   "disambiguate your destination"))
 (check-true
  (string-prefix? (run-t "from N to G") "disambiguate your current location"))
 (check-true
  (string-prefix? (run-t "from N to N") "disambiguate your current location"))
 (check-equal? (run-t "disable blabla") "no such station to disable: blabla")
 (check-true
  (string-prefix?
   (run-t "from Northeastern to Northeastern")
   "Close your eyes"))
 (check-true
  (string-prefix?
   (run-t "from Northeaster to Northeastern")
   "Close your eyes")))

