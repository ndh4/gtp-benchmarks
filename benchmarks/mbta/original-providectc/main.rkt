#lang racket

;; ===================================================================================================
(require
  ;; "run-t.rkt"
  "data.rkt"
  "helpers.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  "../../../ctcs/configurable.rkt"
  (only-in racket/string string-join))
(require/configurable-contract "run-t.rkt" run-t manage EOM DONE ENABLE DISABLE PATH )

(provide/configurable-contract
 [dat->station-names ([max (->i ([fname (and/c path-string? (λ (f) (file-exists? f)))])
                                [result (fname)
                                        (and/c (listof station?)
                                               (λ (lst)
                                                 (sublist? lst (file->lines fname))))])]
                      #;[max/sub1 (-> (and/c string? (λ (f) (file-exists? f)))
                                      (listof station?))]
                      [types (-> string? (listof string?))])]
 [BLUE-STATIONS ([max (and/c (listof station?)
                             (λ (lst)
                               (sublist? lst (file->lines "../base/blue.dat"))))]
                 #;[max/sub1 (listof station?)]
                 [types (listof string?)])]
 [ORANGE-STATIONS ([max (and/c (listof station?)
                               (λ (lst)
                                 (sublist? lst (file->lines "../base/orange.dat"))))]
                   #;[max/sub1 (listof station?)]
                   [types (listof string?)])]
 [path ([max (->i ([from string?]
                   [to string?])
                  [result (from to)
                          (λ (res)
                            (ordered-substrings? (list "from" from "to" to) res))])]
        #;[max/sub1 (->i ([from string?]
                          [to string?])
                         [result (from to)
                                 (λ (res)
                                   (and (substring? from res)
                                        (substring? to res)))])]
        [types (-> string? string? string?)])]
 [enable ([max (->i ([s string?])
                    [result (s)
                            (λ (res)
                              (ordered-substrings? (list "enable" s) res))])]
          #;[max/sub1 (->i ([s string?])
                           [result (s)
                                   (λ (res)
                                     (substring? s res))])]
          [types (-> string? string?)])]
 [disable ([max (->i ([s string?])
                     [result (s)
                             (λ (res)
                               (ordered-substrings? (list "disable" s) res))])]
           #;[max/sub1 (->i ([s string?])
                            [result (s)
                                    (λ (res)
                                      (substring? s res))])]
           [types (-> string? string?)])])


;; ===================================================================================================
(define (dat->station-names fname)
  (for/list ([line (in-list (file->lines fname))]
             #:when (and (< 0 (string-length line))
                         (not (eq? #\- (string-ref line 0)))))
    (string-trim line)))

(define BLUE-STATIONS
  (dat->station-names "../base/blue.dat"))

(define ORANGE-STATIONS
  (dat->station-names "../base/orange.dat"))

;; String String -> String
(define (path from to)
  (format "from ~a to ~a" from to))

;; String -> String
(define (enable s)
  (format "enable ~a" s))

(define (disable s)
  (format "disable ~a" s))

;; ===================================================================================================

(module+ test
  (require rackunit)
  (define (run-query str)
    (define r (run-t str))
    (if r
        r
        (error 'main (format "run-t failed to respond to query ~e\n" str))))
  (define (num-pieces res)
    (length (string-split res "\n")))
  (check-equal? (num-pieces (run-query (path "Airport" "Northeastern"))) 14)
  
  (define res1 (run-query (disable "Government")))
  (check-equal? (num-pieces res1) 1)
  (check-equal? (num-pieces (run-query (path "Airport" "Northeastern"))) 16)
  
  (define res2 (run-query (enable "Government")))
  (check-equal? (num-pieces res2) 1)
  (check-equal? (num-pieces (run-query (path "Airport" "Harvard Square"))) 12)

  (define res3 (run-query (disable "Park Street")))
  (check-equal? (num-pieces res3) 1)
  (check-equal? (num-pieces (run-query (path "Northeastern" "Harvard Square"))) 1) ;;impossible path

  (define res4 (run-query (enable "Park Street")))
  (check-equal? (num-pieces res4) 1)
  (check-equal? (num-pieces (run-query (path "Northeastern" "Harvard Square"))) 12))