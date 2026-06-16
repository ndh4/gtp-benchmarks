#lang racket

(require "../base/my-graph.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         "data.rkt"
         "helpers.rkt"
         racket/contract)

(provide unweighted-graph/directed*
         attach-edge-property*
         in-neighbors*
         SOURCE-DIRECTORY
         COLORS
         line-specification?
         read-t-graph
         read-t-line-from-file
         lines->hash
         mbta%)

(provide mbta%/c)

(define/ctc-helper
 T-graph/c
 (λ (g)
   (and (graph? g)
        (set=? (get-vertices g) expected-stations)
        (> (length (get-edges g)) 200)
        (andmap valid-pair? (get-edges g)))))

(define/ctc-helper
 mbta%/c
 (class/c
  (render (->m (set/c string?) string?))
  (station?
   (->i
    ((self any/c) (name string?))
    (result (name) (and (member name expected-stations) #t))))
  (station
   (->i
    ((self any/c) (name string?))
    (result (or/c station? (listof station?)))
    #:post
    (name result)
    (cond
     ((and (list? result) (> (length result) 1))
      (for/and
       ((s (in-list result)))
       (and (string-contains? s name) (member s expected-stations))))
     ((list? result)
      (and (empty? result)
           (for/and
            ((s (in-list expected-stations)))
            (not (string-contains? s name)))))
     (else (string-contains? result name)))))
  (find-path
   (->i
    ((self any/c) (from station?) (to station?))
    (result (listof (listof (list/c station? (set/c line?)))))
    #:post
    (self from to result)
    (let* ((raw-graph (get-field G self))
           (raw-graph-path (fewest-vertices-path raw-graph from to)))
      (and (implies (not (empty? result)) raw-graph-path)
           (implies raw-graph-path (not (empty? result)))
           (for/and
            ((path (in-list result)))
            (and (equal? (first (first path)) from)
                 (equal? (first (last path)) to)
                 (for/and
                  ((prev-station (in-list path))
                   (next-station (in-list (rest path))))
                  (and (has-edge?
                        raw-graph
                        (first prev-station)
                        (first next-station))
                       (not (set-empty? (second prev-station)))
                       (not (set-empty? (second next-station)))))))))))
  (field (G T-graph/c)
         (stations
          (and/c (listof station?) (λ (s) (set=? s expected-stations))))
         (connection-on (-> station? station? (set/c line?)))
         (bundles
          (and/c
           (listof (list/c color? (set/c line?)))
           (λ (res)
             (andmap
              (λ (lst)
                (lines-in-color-file?
                 (set->list (second lst))
                 (file->lines (format SOURCE-DIRECTORY (first lst)))))
              res)))))))

(define/ctc-helper
 mbta%/type
 (class/c
  (render (->m (set/c string?) string?))
  (station? (->m string? boolean?))
  (station (->m string? (or/c string? (listof string?))))
  (find-path
   (->m string? string? (listof (listof (list/c string? (set/c string?))))))
  (field (G graph?)
         (stations (listof station?))
         (connection-on (-> string? string? (set/c string?)))
         (bundles (listof (list/c string? (set/c string?)))))))

(define/contract
 unweighted-graph/directed*
 (configurable-ctc
  (max (-> (listof (list/c any/c any/c)) any))
  (types (-> (listof (list/c any/c any/c)) any)))
 unweighted-graph/directed)

(define/contract
 attach-edge-property*
 (configurable-ctc
  (max (->* (graph?) (#:init any/c #:for-each any/c) any))
  (types (->* (graph?) (#:init any/c #:for-each any/c) any)))
 attach-edge-property)

(define/contract
 in-neighbors*
 (configurable-ctc (max (-> graph? any/c any)) (types (-> graph? any/c any)))
 in-neighbors)

(define/contract
 SOURCE-DIRECTORY
 (configurable-ctc
  (max (λ (res) (string=? "../base/~a.dat" res)))
  (types string?))
 "../base/~a.dat")

(define/contract
 COLORS
 (configurable-ctc
  (max
   (and/c
    (listof color?)
    (λ (lst)
      (andmap
       (λ (color-file) (file-exists? (format SOURCE-DIRECTORY color-file)))
       lst))))
  (types (listof string?)))
 '("blue" "orange" "green" "red"))

(define/contract
 (line-specification? line)
 (configurable-ctc
  (max
   (->i
    ((s string?))
    (result
     (s)
     (λ (res)
       (if res
         (and (andmap line? res) (andmap (λ (l) (substring? l s)) res))
         (not (substring? "-- " s)))))))
  (types (-> string? (or/c boolean? (listof string?)))))
 (define r (regexp-match #px"--* (.*)" line))
 (and r (string-split (second r))))

(define/contract
 (read-t-graph)
 (configurable-ctc
  (max (-> (instanceof/c mbta%/c)))
  (types (-> (instanceof/c mbta%/type))))
 (define-values
  (all-lines bundles)
  (for/fold
   ((all-lines '()) (all-bundles '()))
   ((color COLORS))
   (define next (read-t-line-from-file color))
   (values
    (append next all-lines)
    (cons (list color (apply set (map first next))) all-bundles))))
 (define connections (apply append (map second all-lines)))
 (define stations
   (set-map
    (for/fold ((s* (set))) ((c connections)) (set-add s* (first c)))
    values))
 (define graph (unweighted-graph/directed* connections))
 (define-values
  (connection-on _ connection-on-set!)
  (attach-edge-property* graph #:init (set)))
 (for
  ((line (in-list all-lines)))
  (define name (first line))
  (define connections* (second line))
  (for
   ((c connections*))
   (define from (first c))
   (define to (second c))
   (connection-on-set! from to (set-add (connection-on from to) name))))
 (new
  mbta%
  (G graph)
  (stations stations)
  (bundles bundles)
  (connection-on connection-on)))

(define/contract
 (read-t-line-from-file line-file)
 (configurable-ctc
  (max
   (->i
    ((lf (λ (lf) (color? lf))))
    (result
     (lf)
     (λ (res)
       (andmap
        (λ (pair)
          (and (line? (first pair))
               (= (remainder (length (second pair)) 2) 0)
               (check-station-pairs? (second pair))))
        res)))))
  (types
   (-> string? (listof (list/c string? (listof (list/c string? string?)))))))
 (define full-path (format SOURCE-DIRECTORY line-file))
 (for/list
  (((name line) (in-hash (lines->hash (file->lines full-path)))))
  (list name (rest line))))

(define/contract
 (lines->hash lines0)
 (configurable-ctc
  (max
   (->i
    ((lines (cons/c #px"--* (.*)" (listof string?))))
    (result
     (lines)
     (λ (h)
       (and (andmap line? (hash-keys h))
            (andmap
             (cons/c string? (listof (list/c station? station?)))
             (hash-values h))
            (= (remainder (length (rest (first (hash-values h)))) 2) 0)
            (check-station-pairs? (rest (first (hash-values h)))))))))
  (types
   (->
    (listof string?)
    (hash/c string? (cons/c string? (listof (list/c string? string?)))))))
 (define names0 (line-specification? (first lines0)))
 (define pred0 (second lines0))
 (define Hlines0
   (make-immutable-hash
    (for/list ((name names0)) (cons name (cons pred0 '())))))
 (let read-t-line ((lines (cddr lines0)) (names names0) (Hlines Hlines0))
   (cond
    ((empty? lines) Hlines)
    (else
     (define current-stop (string-trim (first lines)))
     (cond
      ((line-specification? current-stop)
       =>
       (lambda (names) (read-t-line (rest lines) names Hlines)))
      (else
       (define new-connections
         (for/fold
          ((Hlines1 Hlines))
          ((name (in-list names)))
          (define line (hash-ref Hlines1 name))
          (define predecessor (first line))
          (define connections
            (list*
             (list predecessor current-stop)
             (list current-stop predecessor)
             (rest line)))
          (hash-set Hlines1 name (cons current-stop connections))))
       (read-t-line (rest lines) names new-connections)))))))

(define/contract
 mbta%
 (configurable-ctc (max mbta%/c) (types mbta%/type))
 (class object%
   (init-field G stations connection-on bundles)
   (super-new)
   (define/public
    (render b)
    (define r (memf (lambda (c) (subset? (second c) b)) bundles))
    (if r (first (first r)) (string-join (set-map b values) " ")))
   (define/public
    (station word)
    (define word# (regexp-quote word))
    (define candidates
      (for/list ((s stations) #:when (regexp-match word# s)) s))
    (if (and (cons? candidates) (empty? (rest candidates)))
      (first candidates)
      candidates))
   (define/public (station? s) (cons? (member s stations)))
   (define/public
    (find-path from0 to)
    (define paths* (find-path/aux from0 to))
    (for/list
     ((path paths*))
     (define start (first path))
     (cond
      ((empty? (rest path)) (list (list start (set))))
      (else
       (define next (connection-on start (second path)))
       (define-values
        (_ result)
        (for/fold
         ((predecessor start) (r (list (list start next))))
         ((station (rest path)))
         (values
          station
          (cons (list station (connection-on station predecessor)) r))))
       (reverse result)))))
   (define/private
    (find-path/aux from0 to)
    (let search ((from from0) (visited '()))
      (cond
       ((equal? from to) (list (list from)))
       ((member from visited) (list))
       (else
        (define visited* (cons from visited))
        (for/fold
         ((all-paths '()))
         ((n (in-neighbors* G from)))
         (define paths-from-from-to-to
           (map (lambda (p) (cons from p)) (search n visited*)))
         (append all-paths paths-from-from-to-to))))))))

(module+
 test
 (require rackunit)
 (check-equal? (line-specification? "---- blue") '("blue"))
 (check-equal? (line-specification? "----blue") #f)
 (check-equal? (line-specification? "- blue") '("blue"))
 (check-equal? (line-specification? "- A B C") '("A" "B" "C"))
 (check-exn exn:fail? (λ () (lines->hash '("---- line "))))
 (check-equal?
  '#hash(("line" . ("Davis Station")))
  (lines->hash '("-- line " "Davis Station")))
 (check-equal?
  '#hash(("line"
          .
          ("Davis Station"
           ("Alewife Station" "Davis Station")
           ("Davis Station" "Alewife Station"))))
  (lines->hash '("---- line " "Alewife Station" "Davis Station")))
 (check-equal?
  '#hash(("line1"
          .
          ("Davis Station"
           ("Alewife Station" "Davis Station")
           ("Davis Station" "Alewife Station")))
         ("line2"
          .
          ("Davis Station"
           ("Alewife Station" "Davis Station")
           ("Davis Station" "Alewife Station"))))
  (lines->hash '("---- line1 line2 " "Alewife Station" "Davis Station")))
 (check-exn
  exn:fail?
  (λ ()
    (lines->hash
     '("-- line1 line2 "
       "Alewife Station"
       "---- line3 "
       "Government Center Station"))))
 (check-equal?
  '#hash(("line1"
          .
          ("Government Center Station"
           ("Alewife Station" "Government Center Station")
           ("Government Center Station" "Alewife Station")))
         ("line2"
          .
          ("Davis Station"
           ("Alewife Station" "Davis Station")
           ("Davis Station" "Alewife Station"))))
  (lines->hash
   '("-- line1 line2 "
     "Alewife Station"
     "---- line1 "
     "Government Center Station"
     "---- line2 "
     "Davis Station")))
 (check-equal?
  '(("blue"
     (("Government Center Station" "Bowdoin Station")
      ("Bowdoin Station" "Government Center Station")
      ("State Station" "Government Center Station")
      ("Government Center Station" "State Station")
      ("Aquarium Station" "State Station")
      ("State Station" "Aquarium Station")
      ("Maverick Station" "Aquarium Station")
      ("Aquarium Station" "Maverick Station")
      ("Airport Station" "Maverick Station")
      ("Maverick Station" "Airport Station")
      ("Wood Island Station" "Airport Station")
      ("Airport Station" "Wood Island Station")
      ("Orient Heights Station" "Wood Island Station")
      ("Wood Island Station" "Orient Heights Station")
      ("Suffolk Downs Station" "Orient Heights Station")
      ("Orient Heights Station" "Suffolk Downs Station")
      ("Beachmont Station" "Suffolk Downs Station")
      ("Suffolk Downs Station" "Beachmont Station")
      ("Revere Beach Station" "Beachmont Station")
      ("Beachmont Station" "Revere Beach Station")
      ("Wonderland Station" "Revere Beach Station")
      ("Revere Beach Station" "Wonderland Station"))))
  (read-t-line-from-file "blue"))
 (define graph (read-t-graph))
 (check-equal? (send graph station "Oops") '())
 (check-equal?
  (send graph station "Northeastern University Station")
  "Northeastern University Station")
 (check-equal?
  (send graph station "Northeastern")
  "Northeastern University Station")
 (check-equal?
  (send graph station "Center")
  '("Hynes Convention Center"
    "Government Center Station"
    "Quincy Center Station"
    "Malden Center Station"
    "Tufts Medical Center Station"))
 (check-equal? (send graph station? "Northeastern") #f)
 (check-equal? (send graph station? "Northeastern University Station") #t)
 (check-equal?
  `((("Government Center Station" ,(set))))
  (send graph find-path
    "Government Center Station"
    "Government Center Station"))
 (check-equal?
  `((("Northeastern University Station" ,(set "E"))
     ("Symphony Station" ,(set "E"))))
  (send graph find-path "Northeastern University Station" "Symphony Station"))
 (check-equal?
  `((("Northeastern University Station" ,(set "E"))
     ("Symphony Station" ,(set "E"))
     ("Prudential Station" ,(set "E"))))
  (send graph find-path
    "Northeastern University Station"
    "Prudential Station"))
 (define multiple-routes
   (send graph find-path "Government Center Station" "Haymarket Station"))
 (check-not-false
  (member
   `(("Government Center Station" ,(set "D" "E" "B" "C"))
     ("Park Street Station" ,(set "D" "E" "B" "C"))
     ("Downtown Crossing Station" ,(set "Mattapan" "Braintree"))
     ("State Station" ,(set "orange"))
     ("Haymarket Station" ,(set "orange")))
   multiple-routes))
 (check-not-false
  (member
   `(("Government Center Station" ,(set "D" "E" "B" "C"))
     ("Haymarket Station" ,(set "D" "E" "B" "C")))
   multiple-routes))
 (check-not-false
  (member
   `(("Government Center Station" ,(set "blue"))
     ("State Station" ,(set "blue"))
     ("Haymarket Station" ,(set "orange")))
   multiple-routes)))

