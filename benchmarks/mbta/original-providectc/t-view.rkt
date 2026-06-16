#lang racket

;; implement the view (renderer) for the T path finder

;; (provide 
;;  ;; type Manage = 
;;  ;; (Class 
;;  ;;  ;; disable the given station: #f for sucecss, String for failure
;;  ;;  [add-to-disabled (-> String [Maybe String]]
;;  ;;  ;; enable the given station: #f for sucecss, String for failure
;;  ;;  [remove-from-disabled (-> String [Maybe String]]
;;  ;;  ;; turn the inquiry strings into stations and find a path from the first to the second  
;;  ;;  [find (-> String String String)])
;;  manage%)

;; ===================================================================================================
(require
 ; "t-graph.rkt"
 "../../../ctcs/precision-config.rkt"
 "../../../ctcs/common.rkt"
 "../../../ctcs/configurable.rkt"
 "helpers.rkt"
 (only-in "data.rkt" expected-stations station? line-families)
 (only-in "t-graph.rkt" mbta%/c)
 racket/contract)
(require/configurable-contract "t-graph.rkt" mbta% read-t-graph)

(provide/configurable-contract
 [selector ([max (->i ([inp-lst (and/c cons? (listof (listof any/c)))])
                      [result (inp-lst)
                              (λ (out-lst)
                                (and (list? out-lst)
                                     (cons? (member out-lst inp-lst))
                                     (andmap (lambda (l)
                                               (<= (count-strs-in-lst out-lst)
                                                   (count-strs-in-lst l)))
                                             inp-lst)))])]
            #;[max/sub1 (->i ([inp-lst (listof (listof any/c))])
                             [result (inp-lst)
                                     (λ (out-lst)
                                       (and (list? out-lst)
                                            (cons? (member out-lst inp-lst))))])]
            [types (-> (listof (listof any/c))
                       (listof any/c))])]
 [INTERNAL ([max "find path: it is impossible to get from ~a to ~a [internal error]"]
            [types string?])]
 [CURRENT-LOCATION ([max "disambiguate your current location: ~a"]
                    [types string?])]
 [CURRENT-LOCATION-0 ([max "no such station: ~a"]
                      [types string?])]
 [DESTINATION ([max "disambiguate your destination: ~a"]
               [types string?])]
 [DESTINATION-0 ([max "no such destination: ~a"]
                 [types string?])]
 [NO-PATH ([max "it is currently impossible to reach ~a from ~a via subways"]
           [types string?])]
 [DISABLED ([max "clarify station to be disabled: ~a"]
            [types string?])]
 [ENABLED ([max "clarify station to be enabled: ~a"]
           [types string?])]
 [DISABLED-0 ([max "no such station to disable: ~a"]
              [types string?])]
 [ENABLED-0 ([max "no such station to enable: ~a"]
             [types string?])]
 [ENSURE ([max "---ensure you are on ~a"]
          [types string?])]
 [SWITCH ([max "---switch from ~a to ~a"]
          [types string?])]
 [manage% ([max manage-c/max-ctc]
           #;[max/sub1 manage-c/max/sub1-ctc]
           [types manage-c/types-ctc])])

;; ===================================================================================================
;; [X -> Real] [Listof X] -> X
;; argmax also okay 
;; select an [Listof X] that satisfies certain length criteria
(define (selector l)
  ((curry argmin (lambda (p) (length (filter string? p)))) l)) 

;; ---------------------------------------------------------------------------------------------------

(define INTERNAL
  "find path: it is impossible to get from ~a to ~a [internal error]")

(define CURRENT-LOCATION
  "disambiguate your current location: ~a")

(define CURRENT-LOCATION-0
  "no such station: ~a") 

(define DESTINATION
  "disambiguate your destination: ~a")

(define DESTINATION-0
  "no such destination: ~a")

(define NO-PATH
  "it is currently impossible to reach ~a from ~a via subways")

(define DISABLED
  "clarify station to be disabled: ~a")

(define ENABLED
  "clarify station to be enabled: ~a")

(define DISABLED-0
  "no such station to disable: ~a")

(define ENABLED-0
  "no such station to enable: ~a")

(define ENSURE
  "---ensure you are on ~a")

(define SWITCH
  "---switch from ~a to ~a")

;; ---------------------------------------------------------------------------------------------------

(define/ctc-helper (expected-stations-matching s)
  (filter (λ (other-s) (string-contains? other-s s))
          expected-stations))
(define/ctc-helper (num-expected-stations-matching s)
  (length (expected-stations-matching s)))

(define/ctc-helper stash1 (box #f))
(define/ctc-helper stash2 (box #f))
(define/ctc-helper manage-c/max-ctc
  (class/c
   [add-to-disabled
    (->i ([self any/c]
          [s string?])
         #:pre (self)
         (set-box! stash1 (length (get-field disabled self)))
         [result (s)
                 (cond
                   [(= (num-expected-stations-matching s) 1) #f]
                   [else
                    (and/c string? (λ (res) (substring? s res)))])] ;; FIX: s and res swapped
         #:post (self s result)
         (let ([disabled (get-field disabled self)])
           (and (list? disabled)
                (if (string? result)
                    (= (length disabled)
                       (unbox stash1))
                    (and (> (length disabled)
                            (unbox stash1))
                         (for/or ([other-s disabled])
                           (string-contains? other-s s)))))))]
   [remove-from-disabled
    (->i ([self any/c]
          [s string?])
         #:pre (self)
         (set-box! stash2 (length (get-field disabled self)))
         [result (s)
                 (cond
                   [(= (num-expected-stations-matching s) 1) #f]
                   [else (and/c string? (λ (res) (substring? s res)))])] ;; ANOTHER FIX: same issue as above
         #:post (self s result)
         (let ([disabled (get-field disabled self)])
           (and (list? disabled)
                (if (string? result)
                    (= (length disabled)
                       (unbox stash2))
                    (and (<= (length disabled) ; may not have been disabled in first place, so = OK
                             (unbox stash2))
                         (for/and ([other-s disabled])
                           (not (string-contains? other-s s))))))))]
   [find (->i ([self any/c]
               [from string?]
               [to string?])
              [result (self from to)
                      (λ (res)
                        (correct-find-result? self from to res))])]
   (field [mbta-subways (instanceof/c mbta%/c)]
          [disabled (listof station?)])))

(define/ctc-helper (correct-find-result? self from to res)
  (define from-count (num-expected-stations-matching from))
  (define to-count (num-expected-stations-matching to))
  (define any-disabled? (> (length (get-field disabled self)) 0))
  (and
   ;; base properties of all valid results
   (string? res)
   (or (string-contains? res from)
       (string-contains? res to))
   (cond
     [(and (= from-count 1)
           (= to-count 1)
           (string-contains? res "impossible")
           any-disabled?)
      #t] ;; without reimplementing pathing logic, we'll have to assume the lack of path is OK
     [(or (not (= from-count 1))
          (not (= to-count 1)))
      (or (string-contains? res "disambiguate")
          (string-contains? res "no such"))]
     [else
      (define underlying-graph (get-field G (get-field mbta-subways self)))
      (define path-parts
        (map (λ (line) (if (regexp-match? "---|impossible|tap your heels" line)
                           line
                           (string-split line ", take ")))
             (string-split res "\n")))
      (and (string-contains? res from)
           (string-contains? res to)
           (valid-path? path-parts underlying-graph)
           (sufficient-path? path-parts
                             underlying-graph
                             (first (expected-stations-matching from))
                             (first (expected-stations-matching to))))])))


(define/ctc-helper (valid-path? parts underlying-graph)
  (local-require "../base/my-graph.rkt")
  ;; (listof (or/c "---.*" (list station? line-type?)))
  (for/and ([prev-station (in-list parts)]
            [next-station (in-list (rest parts))])
    (match* {prev-station next-station}
      [{(list p pline)
        (list n nline)}
       (has-edge? underlying-graph p n)]
      ;; lltodo ideally would want to check that lines & switch/ensure messages
      ;; are reasonable (e.g. match up with the prior and next line, that for
      ;; every change of line there is a switch, ...) but switch/ensure messages
      ;; are buggy, so we can only check some small things
      [{(list p pline)
        (? string?)}
       #t]
      [{(regexp #rx"---switch from ([^ ]+) to ([^ ]+)" (list _ from to))
        (list n nline)}
       (not (equal? from to))]
      [{(? string?)
        (list p pline)}
       #t]
      [{_ _}
       #f])))

(define/ctc-helper (sufficient-path? path-parts underlying-graph from to)
  (local-require "../base/my-graph.rkt")
  (define underlying-graph-path (fewest-vertices-path underlying-graph from to))
  (define no-path? (and (= (length path-parts) 1)
                        (string-contains? (first path-parts) "impossible")))
  (define self-path? (and (= (length path-parts) 1)
                          (string-contains? (first path-parts) "tap your heels")))
  (and (or no-path?
           self-path?
           (and (equal? (first (first path-parts)) from)
                (equal? (first (last path-parts)) to)))
       (implies no-path?
                (not underlying-graph-path))
       (implies underlying-graph-path
                (not no-path?))))

(define/ctc-helper manage-c/types-ctc
  (class/c
     (add-to-disabled (->m string? (or/c string? #f)))
     (remove-from-disabled (->m string? (or/c string? #f)))
     (find (->m string? string? string?))
     (field [mbta-subways (is-a?/c mbta%)]
            [disabled list?])))


(define manage%
  (class object% 
    (super-new)
    
    (field 
     ;; [instance-of MBTA%]
     [mbta-subways (read-t-graph)]
     ;; [Listof Station]
     [disabled '()])
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (add-to-disabled s)
      (define station (send mbta-subways station s))
      (cond
        [(string? station) (set! disabled (cons station disabled)) #f]
        [(empty? station) (format DISABLED-0 s)]
        [else (format DISABLED (string-join station))]))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (remove-from-disabled s)
      (define station (send mbta-subways station s))
      (cond
        [(string? station) (set! disabled (remove* (list station) disabled)) #f]
        [(empty? station) (format ENABLED-0 s)]
        [else (format ENABLED (string-join station))]))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (find from to)
      (define from-station (send mbta-subways station from))
      (define to-station   (send mbta-subways station to))
      (cond
        [(and (string? from-station)
              (string? to-station)
              (string=? from-station to-station))
         (format "Close your eyes and tap your heels three times. Open your eyes. You will be at ~a." to)]
        [(cons? from-station) (format CURRENT-LOCATION (string-join from-station))]
        [(cons? to-station)   (format DESTINATION (string-join to-station))]
        [(empty? from-station) (format CURRENT-LOCATION-0 from)]
        [(empty? to-station)   (format DESTINATION-0 to)]
        [else 
         (define paths (send mbta-subways find-path from-station to-station))
         (define path* (removed-paths-with-disabled-stations paths))
         (cond
           [(empty? paths) (format INTERNAL from-station to-station)]
           [(empty? path*) (format NO-PATH to-station from-station)]
           [else 
            (define paths-with-switch (for/list ([p path*]) (insert-switch p)))
            (define best-path-as-string*
              (for/list ([station-or-comment (pick-best-path paths-with-switch)])
               (match station-or-comment
                 [`(,name ,line) (string-append name ", take " (send mbta-subways render line))]
                 [(? string? comment) comment])))
            (string-join best-path-as-string* "\n")])]))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/private (removed-paths-with-disabled-stations paths*)
      (for/list ([p paths*] 
                 #:unless ;; any of the disabled stations is on the path 
                 (let ([stations (map first p)]) 
                   (for/or ((s stations)) (member s disabled))))
        p))
    
    ;; type Path* ~~ Path with "switch from Line to Line" strings in the middle 
    
    ;; -----------------------------------------------------------------------------------------------
    ;; [Listof Path*] -> Path*
    (define/private (pick-best-path paths*)
      (selector paths*))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Path -> Path* 
    (define/private (insert-switch path0)  
      (define start (first path0))
      (define pred-lines0 (second start))
      (define pred-string0 (send mbta-subways render pred-lines0))
      (cons start
            (let loop ([pred-lines pred-lines0][pred-string pred-string0][path (rest path0)])
              (cond
                [(empty? path) '()]
                [else 
                 (define stop (first path))
                 (define name (first stop))
                 (define stop-lines (second stop))
                 (define stop-string (send mbta-subways render stop-lines))
                 (define remainder (loop stop-lines stop-string (rest path)))
                 (cond
                   [(proper-subset? stop-lines pred-lines)
                    (list* (format ENSURE stop-string) stop remainder)]
                   [(set-empty? (set-intersect stop-lines pred-lines)) 
                    (list* (format SWITCH pred-string stop-string) stop remainder)]
                   [else (cons stop remainder)])]))))))

#;(define/ctc-helper find-method-ctc
  (->i ([this any/c]
        [from string?]
        [to string?])
       [result (from to)
               (λ (res)
                 (let ([from-station (send (read-t-graph) station from)]
                       [to-station (send (read-t-graph) station to)])
                   (cond
                     [(string=? from to) (substring? to res)]
                     [(cons? from-station) (substring? (string-join from-station) res)]
                     [(cons? to-station) (substring? (string-join to-station) res)]
                     [(empty? from-station) (substring? from res)]
                     [(empty? to-station) (substring? to res)]
                     [else (and (substring? from res)
                                (substring? to res))])))]))


(provide manage-c/max-ctc
         manage-c/types-ctc)

;; Testing ==============================================================================

(module+ test
  (require rackunit)
  (define manage1 (new manage%))
  (define manage1_disable (get-field disabled manage1))
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
  (check-equal? "no such station: brook" (send manage1 find "brook" "brook"))
  (check-equal? "disambiguate your current location: Brookline Hills Station Stony Brook Station Brookline Village Station" (send manage1 find "Brook" "Brook"))
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
