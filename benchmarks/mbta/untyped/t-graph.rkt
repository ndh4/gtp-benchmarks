#lang racket
;; implements the model for the T path finder 

;; (provide 
;;  ;; type MBTA% = 
;;  ;; (Class mbta% 
;;  ;;        [find-path (-> Station Station [Listof Path])] 
;;  ;;        [render (-> [Setof Station] String)
;;  ;;        [station?  (-> String Boolean)]
;;  ;;        [station   (-> String (U Station [Listof Station])])
;;  ;; type Path  = [Listof [List Station [Setof Line]]]
;;  ;; interpretation: take the specified lines to the next station from here 
;;  ;; type Station = String
;;  ;; type Line is one of: 
;;  ;; -- E
;;  ;; -- D 
;;  ;; -- C
;;  ;; -- B 
;;  ;; -- Mattapan
;;  ;; -- Braintree
;;  ;; -- orange
;;  ;; -- blue 
;;  ;; as Strings
 
;;  ;; ->* [instance-of MBTA%]
;;  ;; read the specification of the T map from file and construct an object that can
;;  ;; -- convert a string to a (list of) station(s) 
;;  ;; -- find a path from one station to another
;;  read-t-graph
;;  mbta%)

;; ===================================================================================================
(require "../base/my-graph.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         "data.rkt"
         "helpers.rkt"
         racket/contract)

(provide/configurable-contract
 #;[unweighted-graph/directed* ([max ((listof (list/c any/c any/c)) . -> . any)]
                              [types ((listof (list/c any/c any/c)) . -> . any)])]
 #;[attach-edge-property* ([max ([graph?]
                               [#:init any/c
                                #:for-each any/c]
                               . ->* .
                               any)]
                         [types ([graph?]
                                 [#:init any/c
                                  #:for-each any/c]
                                 . ->* .
                                 any)])]
 #;[in-neighbors* ([max (graph? any/c . -> . any)]
                 [types (graph? any/c . -> . any)])]
 #;[SOURCE-DIRECTORY ([max (λ (res)
                           (string=? "../base/~a.dat" res))]
                    #;[max/sub1 (and/c string?
                                       (λ (s)
                                         (let ([split (string-split s ".")])
                                           (string=? "dat"
                                                     (list-ref split (- (length split) 1))))))]                                           
                    [types string?])]
 #;[COLORS ([max (and/c (listof color?)
                      (λ (lst)
                        (andmap (λ (color-file)
                                  (file-exists? (format SOURCE-DIRECTORY color-file)))
                                lst)))]
          #;[max/sub1 (listof color?)]
          [types (listof string?)])]
 #;[line-specification? ([max (->i ([s string?])
                                 [result (s)
                                         (λ (res)
                                           (if res
                                               (and (andmap line? res)
                                                    (andmap (λ (l) (substring? l s)) res))
                                               (not (substring? "--" s))))])]
                       #;[max/sub1 (->i ([s string?])
                                        [result (s)
                                                (λ (res)
                                                  (if res
                                                      (< (length res) (string-length s))
                                                      (not (substring? "-- " s))))])]                      
                       [types (-> string? (or/c boolean? (listof string?)))])]
 [read-t-graph (;; not the strongest contract I can think of here
                ;; maybe you could check if find-path returns all valid paths
                [max (-> (instanceof/c mbta%/c))]
                #;[max/sub1 (-> (object/c
                                 (render (->m (set/c string?) string?))
                                 (station? (->m string? boolean?))
                                 (station (->m string? (or/c station? (listof station?))))
                                 (find-path (->m station? station?
                                                 (listof (listof (list/c station? (set/c line?))))))))]
                [types (-> (instanceof/c mbta%/type))])]
 #;[read-t-line-from-file ([max (->i ([lf (λ (lf) (color? lf))])
                                   [result (lf)
                                           (λ (res)
                                             (andmap (λ (pair)
                                                       (and (line? (first pair))
                                                            (= (remainder (length (second pair)) 2) 0)
                                                            (check-station-pairs? (second pair))))
                                                     res))])]
                         #;[max/sub1 (->i ([lf (λ (lf) (color? lf))])
                                          [result (lf)
                                                  (λ (res)
                                                    (andmap (λ (pair)
                                                              (and (line? (first pair))
                                                                   (= (remainder (length (second pair)) 2) 0)))
                                                            res))])]                 
                         [types (-> string?
                                    (listof (list/c string?
                                                    (listof (list/c string? string?)))))])]
 #;[lines->hash ([max (->i ([lines (listof string?)])
                         [result (lines)
                                 ;; ll: checked 4x per unique line
                                 (λ (h)
                                   (and (andmap line? (hash-keys h))
                                        (andmap (cons/c string? (listof (list/c station? station?)))
                                                (hash-values h))
                                        (= (remainder (length (rest (first (hash-values h)))) 2) 0)
                                        (check-station-pairs? (rest (first (hash-values h))))))])]
               #;[max/sub1 (->i ([lines (listof string?)])
                                [result (lines)
                                        (λ (h)
                                          (and (andmap line? (hash-keys h))
                                               (andmap (cons/c string? (listof (list/c station? station?)))
                                                       (hash-values h))
                                               (= (remainder (length (rest (first (hash-values h)))) 2) 0)))])]

               [types (-> (listof string?)
                          (hash/c string?
                                  (cons/c string?
                                          (listof (list/c string? string?)))))])]
 [mbta% ([max mbta%/c]
         #;[max/sub1
            (class/c
             (render (->m (set/c string?) string?))
             (station? (->m string? boolean?))
             (station (->m string? (or/c station? (listof station?))))
             (find-path (->m station? station?
                             (listof (listof (list/c station? (set/c line?))))))
             (field [G graph?]
                    [stations (listof station?)]
                    [connection-on (-> station? station? (set/c line?))]
                    [bundles (listof (list/c color? (set/c line?)))]))]
         [types mbta%/type])])

(provide mbta%/c)

(define/ctc-helper T-graph/c
  (λ (g)
    (and (graph? g)
         (set=? (get-vertices g) expected-stations)
         (> (length (get-edges g)) 200) ;; uhh I dunno there should be a lot
         (andmap valid-pair? (get-edges g)))))

(define/ctc-helper mbta%/c
  (class/c
   (render (->m (set/c string?) string?))
   (station? (->i ([self any/c]
                   [name string?])
                  [result (name)
                          (and (member name expected-stations) #t)]))
   (station (->i ([self any/c]
                  [name string?])
                 [result (or/c station? (listof station?))]
                 #:post {name result}
                 (or (not (empty? result))
                     (for/and ([s (in-list expected-stations)])
                       (not (string-contains? s name))))))
   (find-path (->i ([self any/c]
                    [from station?]
                    [to station?])
                   [result (listof (listof (list/c station? (set/c line?))))]
                   #:post {self from to result}
                   (let* ([raw-graph (get-field G self)]
                          [raw-graph-path (fewest-vertices-path raw-graph from to)])
                     (and (implies (not (empty? result))
                                   raw-graph-path)
                          (implies raw-graph-path
                                   (not (empty? result)))
                          (for/and ([path (in-list result)])
                            (and (equal? (first (first path)) from)
                                 (equal? (first (last path)) to)
                                 (for/and ([prev-station (in-list path)]
                                           [next-station (in-list (rest path))])
                                   (has-edge? raw-graph (first prev-station) (first next-station)))))))))
   (field [G T-graph/c]
          [stations (and/c (listof station?) (λ (s) (set=? s expected-stations)))]
          [connection-on (-> station? station? (set/c line?))]
          [bundles (and/c (listof (list/c color? (set/c line?)))
                          (λ (res)
                            (andmap (λ (lst)
                                      (lines-in-color-file?
                                       (set->list(second lst))
                                       (file->lines (format SOURCE-DIRECTORY (first lst)))))
                                    res)))])))

(define/ctc-helper mbta%/type
  (class/c
   (render (->m (set/c string?) string?))
   (station? (->m string? boolean?))
   (station (->m string? (or/c string? (listof string?))))
   (find-path (->m string? string?
                   (listof (listof (list/c string? (set/c string?))))))
   (field [G graph?]
          [stations (listof station?)]
          [connection-on (-> string? string? (set/c string?))]
          [bundles (listof (list/c string? (set/c string?)))])))


(define unweighted-graph/directed*
  unweighted-graph/directed)
(define attach-edge-property*
  attach-edge-property)
(define in-neighbors*
  in-neighbors)




;; type Lines       = [Listof [List String Connections]]
;; type Connections = [Listof Connection]
;; type Connection  = [List Station Station]

(define SOURCE-DIRECTORY
  "../base/~a.dat")

(define COLORS
  '("blue" "orange" "green" "red"))

;; ---------------------------------------------------------------------------------------------------
;; String -> [Maybe [Listof String]]
(define (line-specification? line)
  (define r (regexp-match #px"--* (.*)" line))
  (and r (string-split (second r))))

#| ASSUMPTIONS about source files:
   A data file has the following format: 
   LineSpecification 
   [Station
    | 
    LineSpecification
    ]* 
   A LineSpecification consists of dashes followed by the name of lines, separated by blank spaces. 
   A Station is the string consisting of an entire line, minus surrounding blank spaces. 
|#

;; ---------------------------------------------------------------------------------------------------
(define (read-t-graph)
  (define-values (all-lines bundles)
    (for/fold ((all-lines '()) (all-bundles '())) ((color COLORS))
      (define next (read-t-line-from-file color))
      (values (append next all-lines) (cons (list color (apply set (map first next))) all-bundles))))
  
  (define connections (apply append (map second all-lines)))
  (define stations (set-map (for/fold ((s* (set))) ((c connections)) (set-add s* (first c))) values))
  
  (define graph (unweighted-graph/directed* connections))
  (define-values (connection-on _  connection-on-set!) (attach-edge-property* graph #:init (set)))
  (for ((line (in-list all-lines)))
    (define name (first line))
    (define connections* (second line))
    (for ((c connections*))
      (define from (first c))
      (define to (second c))
      (connection-on-set! from to (set-add (connection-on from to) name))))
  
  (new mbta% [G graph][stations stations][bundles bundles][connection-on connection-on]))

;; ---------------------------------------------------------------------------------------------------
;; String[name of line ~ stem of filename] -> Lines
(define (read-t-line-from-file line-file)
  (define full-path (format SOURCE-DIRECTORY line-file))
  (for/list ([(name line) (in-hash (lines->hash (file->lines full-path)))])
    (list name (rest line))))

;; ---------------------------------------------------------------------------------------------------
;; [Listof String] -> [Hashof String [Cons String [Listof Connections]]]
(define (lines->hash lines0)
  (define names0 (line-specification? (first lines0)))
  (define pred0  (second lines0))
  (define Hlines0 (make-immutable-hash (for/list ([name names0]) (cons name (cons pred0 '())))))
  (let read-t-line ([lines (cddr lines0)][names names0][Hlines Hlines0])
    (cond
      [(empty? lines) Hlines]
      [else 
       (define current-stop (string-trim (first lines)))
       (cond
         [(line-specification? current-stop) 
          => 
          (lambda (names) (read-t-line (rest lines) names Hlines))]
         [else 
          (define new-connections
            (for/fold ([Hlines1 Hlines]) ([name (in-list names)])
              (define line (hash-ref Hlines1 name))
              (define predecessor (first line))
              (define connections 
                (list* (list predecessor current-stop)
                       (list current-stop predecessor)
                       (rest line)))
              (hash-set  Hlines1 name (cons current-stop connections))))
          (read-t-line (rest lines) names new-connections)])])))

;; ---------------------------------------------------------------------------------------------------
(define mbta%
  (class object% 
    (init-field
     ;; Graph 
     G
     ;; [Listof Station]
     stations
     ;; [Station Station -> Line]
     connection-on 
     ;; [Listof [List String [Setof Line]]]
     bundles)
    
    (super-new)
    
    (define/public (render b)
      ;; lltodo fix?
      ;; (define rs (indexes-where bundles (lambda (c) (subset? (second c) b))))
      ;; (if (empty? rs) (string-join (set-map b values) " ") (string-join (map (λ (i) (first (list-ref bundles i)))
      ;;                                                                        rs)
      ;;                                                                   " or "))
      (define r (memf (lambda (c) (subset? (second c) b)) bundles))
      (if r (first (first r)) (string-join (set-map b values) " ")))

    (define/public (station word)
      (define word# (regexp-quote word))
      (define candidates
        (for/list ([s stations] #:when (regexp-match word# s))
          s))
      (if (and (cons? candidates) (empty? (rest candidates)))
          (first candidates)
          candidates))
    
    (define/public (station? s)
      (cons? (member s stations)))
    
    (define/public (find-path from0 to)
      (define paths* (find-path/aux from0 to))
      (for/list ((path paths*))
        (define start (first path))
        (cond
          [(empty? (rest path)) (list start (set))]
          [else             
           (define next (connection-on start (second path)))
           (define-values (_ result)
             (for/fold ([predecessor start][r (list (list start next))]) ((station (rest path)))
               (values station (cons (list station (connection-on station predecessor)) r))))
           (reverse result)])))
    
    ;; Node Node -> [Listof Path]
    (define/private (find-path/aux from0 to)
      (let search ([from from0][visited '()])
        (cond
          [(equal? from to) (list (list from))]
          [(member from visited) (list)]
          [else
           (define visited* (cons from visited))
           (for/fold ((all-paths '())) ([n (in-neighbors* G from)])
             (define paths-from-from-to-to
               (map (lambda (p) (cons from p)) (search n visited*)))
             (append all-paths paths-from-from-to-to))])))))
