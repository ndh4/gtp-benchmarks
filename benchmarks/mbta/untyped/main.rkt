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

;; (provide/configurable-contract
;;  [dat->station-names ([max (->i ([fname (and/c string? (λ (f) (file-exists? f)))])
;;                                 [result (fname)
;;                                         (and/c (listof station?)
;;                                                (λ (lst)
;;                                                  (sublist? lst (file->lines fname))))])]            
;;                       #;[max/sub1 (-> (and/c string? (λ (f) (file-exists? f)))
;;                                       (listof station?))]             
;;                       [types (-> string? (listof string?))])]
;;  [BLUE-STATIONS ([max (and/c (listof station?)
;;                              (λ (lst)
;;                                (sublist? lst (file->lines "../base/blue.dat"))))]
;;                  #;[max/sub1 (listof station?)]
;;                  [types (listof string?)])]
;;  [ORANGE-STATIONS ([max (and/c (listof station?)
;;                                (λ (lst)
;;                                  (sublist? lst (file->lines "../base/orange.dat"))))]
;;                    #;[max/sub1 (listof station?)]
;;                    [types (listof string?)])]
;;  [path ([max (->i ([from string?]
;;                    [to string?])
;;                   [result (from to)
;;                           (λ (res)
;;                             (ordered-substrings? (list "from" from "to" to) res))])]
;;         #;[max/sub1 (->i ([from string?]
;;                           [to string?])
;;                          [result (from to)
;;                                  (λ (res)
;;                                    (and (substring? from res)
;;                                         (substring? to res)))])]
;;         [types (-> string? string? string?)])]
;;  [enable ([max (->i ([s string?])
;;                     [result (s)
;;                             (λ (res)
;;                               (ordered-substrings? (list "enable" s) res))])]
;;           #;[max/sub1 (->i ([s string?])
;;                            [result (s)
;;                                    (λ (res)
;;                                      (substring? s res))])]
;;           [types (-> string? string?)])]
;;  [disable ([max (->i ([s string?])
;;                      [result (s)
;;                              (λ (res)
;;                                (ordered-substrings? (list "disable" s) res))])]
;;            #;[max/sub1 (->i ([s string?])
;;                             [result (s)
;;                                     (λ (res)
;;                                       (substring? s res))])]
;;            [types (-> string? string?)])]
;;  [assert (string? natural? . -> . void?)]
;;  [main any/c])


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

(define (assert result expected-length)
  (define num-result (length (string-split result "\n")))
  (unless (= num-result expected-length)
    (error (format "Expected ~a results, got ~a\nFull list:~a"
                   expected-length
                   num-result
                   result))))

(define (main)
  (define (run-query str)
    (define r (run-t str))
    (if r
        r
        (error 'main (format "run-t failed to respond to query ~e\n" str))))
  (assert (run-query (path "Airport" "Northeastern")) 14)
  (assert (run-query (disable "Government")) 1)
  (assert (run-query (path "Airport" "Northeastern")) 16)
  (assert (run-query (enable "Government")) 1)
  (assert (run-query (path "Airport" "Harvard Square")) 12)
  (assert (run-query (disable "Park Street")) 1)
  (assert (run-query (path "Northeastern" "Harvard Square")) 1) ;;impossible path
  (assert (run-query (enable "Park Street")) 1)
  (assert (run-query (path "Northeastern" "Harvard Square")) 12)
  ;; --
  (for* ([s1 (in-list ORANGE-STATIONS)] [s2 (in-list BLUE-STATIONS)])
    (run-query (path s1 s2))))

(time (main))


;; Testing ==============================================================================

(module+ test
  (require rackunit)
  (require "t-view.rkt")
  (define manage1 (new manage%))
  (define manage1_disable (get-field disabled manage1))
  (check-not-exn (λ () (main)))
  ;; add one station to the disabled list
  (check-equal? '() manage1_disable)
  (define dis_add_out1 (send manage1 add-to-disabled "Government"))
  (check-equal? dis_add_out1 #f)
  (check-equal? '("Government Center Station") (get-field disabled manage1))

  ;; remove something not on the disabled list, while it has contents in it
  (define dis_rem_out1 (send manage1 remove-from-disabled "Haymarket"))
  (check-equal? dis_rem_out1 #f)
  (check-equal? '("Government Center Station") (get-field disabled manage1))

  ;; remove something that exists in the disabled list
  (define dis_rem_out2 (send manage1 remove-from-disabled "Government Center"))
  (check-equal? dis_rem_out2 #f)
  (check-equal? '() (get-field disabled manage1))

  ;; try to remove something from the list when nothing is in it
  (define dis_rem_out3 (send manage1 remove-from-disabled "Government Center"))
  (check-equal? dis_rem_out3 #f)
  (check-equal? '() (get-field disabled manage1))
  ;(printf "dis_rem_out3: ~a\n" dis_rem_out3)

  ;; add two stations to the disabled list

  ;; DISABLED List Behavior
  ;; -- checks that if locations are added or removed from the disabled list are actually viabled
  ;; -- tests that the disabled list doesn't connect to anything
  ;; -- checks for incorrect inputs names into the disabled list, both putting them in and having as a real destination
  ;; -- verifies the disabled list changes when appriorate
  (check-equal? (length (get-field disabled manage1)) 0)
  (define dis_rem_out4 (send manage1 add-to-disabled "Government"))
  (check-equal? (length (get-field disabled manage1)) 1)
  (define dis_rem_out4_2 (send manage1 add-to-disabled "Riverway"))
  (check-equal? dis_rem_out4 dis_rem_out4_2)
  (check-equal? dis_rem_out4 #f)
  (check-equal? #t (boolean? dis_rem_out4))
  (check-equal? '("Riverway Station" "Government Center Station") (get-field disabled manage1))
  (check-equal? (length (get-field disabled manage1)) 2)
  (check-equal? "it is currently impossible to reach Government Center Station from Riverway Station via subways" (send manage1 find "Riverway" "Government"))
  (check-equal? "it is currently impossible to reach Bowdoin Station from Government Center Station via subways" (send manage1 find "Government" "Bowdoin"))
  (check-equal? "it is currently impossible to reach Government Center Station from Bowdoin Station via subways" (send manage1 find "Bowdoin" "Government"))
  (check-equal? "it is currently impossible to reach Bowdoin Station from Riverway Station via subways" (send manage1 find "Riverway" "Bowdoin"))
  (check-equal? "it is currently impossible to reach Riverway Station from Bowdoin Station via subways" (send manage1 find "Bowdoin" "Riverway"))
  (check-equal? "it is currently impossible to reach Suffolk Downs Station from Bowdoin Station via subways" (send manage1 find "Bowdoin" "Suffolk"))
  (define attempt (send manage1 remove-from-disabled "Government"))
  (check-equal? (length (get-field disabled manage1)) 1)
  (check-equal? '("Riverway Station") (get-field disabled manage1))
  (check-equal? "Close your eyes and tap your heels three times. Open your eyes. You will be at Government." (send manage1 find "Government" "Government"))
  (check-equal? "it is currently impossible to reach Government Center Station from Riverway Station via subways" (send manage1 find "Riverway" "Government"))
  (check-equal? "it is currently impossible to reach Riverway Station from Government Center Station via subways" (send manage1 find "Government" "Riverway"))
  (check-equal? "it is currently impossible to reach Bowdoin Station from Riverway Station via subways" (send manage1 find "Riverway" "Bowdoin"))
  (check-equal? "it is currently impossible to reach Riverway Station from Bowdoin Station via subways" (send manage1 find "Bowdoin" "Riverway"))
  (check-equal? "Government Center Station, take blue\nBowdoin Station, take blue" (send manage1 find "Government" "Bowdoin"))
  (check-equal? "Bowdoin Station, take blue\nGovernment Center Station, take blue" (send manage1 find "Bowdoin" "Government"))
  (check-equal? (send manage1 add-to-disabled "brooke") "no such station to disable: brooke")
  (check-equal? (length (get-field disabled manage1)) 1)
  (check-equal? (send manage1 remove-from-disabled "brooke") "no such station to enable: brooke")
  (check-equal? (length (get-field disabled manage1)) 1)
  (check-equal? "no such destination: brooke" (send manage1 find "Government" "brooke"))
  (check-equal? "no such destination: brooke" (send manage1 find "Riverway" "brooke"))
  (check-equal? "clarify station to be disabled: Brookline Hills Station Stony Brook Station Brookline Village Station" (send manage1 add-to-disabled "Brook"))
  (check-equal? #t (string? (send manage1 add-to-disabled "Brook")))
  (check-equal? (length (get-field disabled manage1)) 1)
  (check-equal? '("Riverway Station") (get-field disabled manage1))
  (define dis_rem_out5_1 (send manage1 add-to-disabled "Brookline Hills"))
  (check-equal? (length (get-field disabled manage1)) 2)
  (check-equal? '("Brookline Hills Station" "Riverway Station") (get-field disabled manage1))
  (define dis_rem_out5_2 (send manage1 add-to-disabled "Stony Brook"))
  (check-equal? (length (get-field disabled manage1)) 3)
  (check-equal? '("Stony Brook Station" "Brookline Hills Station" "Riverway Station") (get-field disabled manage1))
  (define dis_rem_out5_3 (send manage1 add-to-disabled "Brookline Village"))
  (check-equal? (length (get-field disabled manage1)) 4)
  (check-equal? '("Brookline Village Station" "Stony Brook Station" "Brookline Hills Station" "Riverway Station") (get-field disabled manage1))
  (check-equal? "Close your eyes and tap your heels three times. Open your eyes. You will be at brook." (send manage1 find "brook" "brook"))
  (check-equal? "Close your eyes and tap your heels three times. Open your eyes. You will be at Brook." (send manage1 find "Brook" "Brook"))
  (check-equal? "Close your eyes and tap your heels three times. Open your eyes. You will be at Riverway." (send manage1 find "Riverway" "Riverway"))
  (check-equal? "no such destination: brookline hills" (send manage1 find "Stony Brook" "brookline hills"))
  (check-equal? "it is currently impossible to reach Brookline Hills Station from Stony Brook Station via subways" (send manage1 find "Stony Brook" "Brookline Hills"))
  (define ts1 (send manage1 add-to-disabled "Riverway"))
  (check-equal? #f ts1)
  (check-equal? (length (get-field disabled manage1)) 5)
  (check-equal? '("Riverway Station" "Brookline Village Station" "Stony Brook Station" "Brookline Hills Station" "Riverway Station") (get-field disabled manage1))
  (define ts2 (send manage1 remove-from-disabled "Riverway"))
  (check-equal? #f ts2)
  (check-equal? #t (and (boolean? (send manage1 remove-from-disabled "Riverway")) (equal? #f (send manage1 remove-from-disabled "Riverway"))))
  (check-equal? (length (get-field disabled manage1)) 3)
  (check-equal? '("Brookline Village Station" "Stony Brook Station" "Brookline Hills Station") (get-field disabled manage1))
  (define ts3 (send manage1 remove-from-disabled "Riverway"))
  (check-equal? #f ts3)
  (check-equal? (length (get-field disabled manage1)) 3)
  (check-equal? '("Brookline Village Station" "Stony Brook Station" "Brookline Hills Station") (get-field disabled manage1))
  (check-equal? "no such station to enable: brook" (send manage1 remove-from-disabled "brook"))
  (check-equal? #t (string? (send manage1 remove-from-disabled "brook")))
  (check-equal? (length (get-field disabled manage1)) 3)
  (check-equal? '("Brookline Village Station" "Stony Brook Station" "Brookline Hills Station") (get-field disabled manage1))

  ;; not really part of test 1, but builds on what the test suite was doing by verifying what incorrect inputs trigger different error messages
  (check-equal? '("Brookline Village Station" "Stony Brook Station" "Brookline Hills Station") (get-field disabled manage1))
  (check-equal? "disambiguate your destination: Brookline Hills Station Stony Brook Station Brookline Village Station" (send manage1 find "Riverway" "Brook"))
  (check-equal? "disambiguate your current location: Cleveland Circle Station Brigham Circle Station" (send manage1 find "Circle" "Brook"))
  (check-equal? "disambiguate your current location: Cleveland Circle Station Brigham Circle Station" (send manage1 find "Circle" "brook"))
  (check-equal? "disambiguate your destination: Brookline Hills Station Stony Brook Station Brookline Village Station" (send manage1 find "riverway" "Brook"))
  (check-equal? (length (get-field disabled manage1)) 3)
  (check-equal? "no such destination: brook" (send manage1 find "Riverway" "brook"))
  (check-equal? "no such station: riverway" (send manage1 find "riverway" "brook"))
  (check-equal? "no such station to disable: brook" (send manage1 add-to-disabled "brook"))
  (check-equal? (length (get-field disabled manage1)) 3)


  ;;;;;;;;;

  (define ts4 (send manage1 add-to-disabled "Sym"))
  (check-equal? #f ts4)
  (check-equal? (length (get-field disabled manage1)) 4)
  (check-equal? '("Symphony Station" "Brookline Village Station" "Stony Brook Station" "Brookline Hills Station") (get-field disabled manage1))
  (define ts5 (send manage1 remove-from-disabled "phony"))
  (check-equal? #f ts5);; lowercase situation that is valid; substring needs to be exactly what is in the data.rkt for it to work properly
  (check-equal? (length (get-field disabled manage1)) 3)
  (check-equal? '("Brookline Village Station" "Stony Brook Station" "Brookline Hills Station") (get-field disabled manage1))
)
