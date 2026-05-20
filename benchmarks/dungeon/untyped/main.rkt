#lang racket

(require require-typed-check
         racket/class
         "../base/un-types.rkt"
         racket/match
         racket/contract
         (only-in "../../../ctcs/common.rkt" or-#f/c)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/configurable.rkt")

(require (only-in racket/set set-intersect))

(require (only-in racket/dict dict-set))

(require (only-in "cell.rkt" cell%? cell%/c))

(require (only-in
          "cell.rkt"
          cell%
          empty-cell%
          south-tee-wall%
          east-tee-wall%
          west-tee-wall%
          north-tee-wall%
          south-east-wall%
          south-west-wall%
          north-east-wall%
          north-west-wall%
          vertical-wall%
          pillar%
          four-corner-wall%
          horizontal-wall%
          horizontal-door%
          vertical-door%
          door%
          wall%
          void-cell%))

(require (only-in
          "grid.rkt"
          array-coord?
          arrayof
          grid?
          within-grid/c
          direction?))

(require (only-in
          "grid.rkt"
          within-grid?
          build-array
          array-set!
          show-grid
          grid-width
          grid-height
          grid-ref
          down
          up
          right
          left))

(require (only-in "utils.rkt" random-result-between/c))

(require (only-in "utils.rkt" reset! random-from random-between))

(provide N
         wall-cache
         free-cache
         animate-generation?
         ITERS
         dungeon-height
         dungeon-width
         try-add-rectangle
         commit-room
         random-direction
         horizontal?
         vertical?
         new-room
         new-corridor
         generate-dungeon
         counts-as-free?
         hash-clear!
         smooth-walls
         smooth-single-wall
         LOOPS
         main)

(define/ctc-helper (alistof key/c val/c) (listof (cons/c key/c val/c)))

(struct room (height width poss->cells free-cells extension-points) #:mutable)

(define/ctc-helper
 (room-with/c height/c width/c poss->cells/c free-cells/c extension-points/c)
 (struct/c
  room
  (and/c index? height/c)
  (and/c index? width/c)
  (and/c (alistof array-coord? cell%/c) poss->cells/c)
  (and/c (listof array-coord?) free-cells/c)
  (and/c (listof array-coord?) extension-points/c)))

(define/ctc-helper any-room? (room-with/c any/c any/c any/c any/c any/c))

(define/contract
 N
 (configurable-ctc
  (max exact-nonnegative-integer?)
  (types exact-nonnegative-integer?))
 1)

(define/contract
 wall-cache
 (configurable-ctc (max (hash/c array-coord? boolean?)) (types hash?))
 (make-hash))

(define/contract
 free-cache
 (configurable-ctc (max (hash/c array-coord? boolean?)) (types hash?))
 (make-hash))

(define/contract
 animate-generation?
 (configurable-ctc (max boolean?) (types boolean?))
 #f)

(define/contract
 ITERS
 (configurable-ctc
  (max exact-nonnegative-integer?)
  (types exact-nonnegative-integer?))
 10)

(define/contract
 dungeon-height
 (configurable-ctc
  (max exact-nonnegative-integer?)
  (types exact-nonnegative-integer?))
 18)

(define/contract
 dungeon-width
 (configurable-ctc
  (max exact-nonnegative-integer?)
  (types exact-nonnegative-integer?))
 60)

(define/ctc-helper
 (room-bounds x y height width direction)
 (define-values
  (min-x max-x)
  (match
   direction
   ((== down) (values x (+ x (sub1 height))))
   ((== up) (values (add1 (- x height)) x))
   (else (values (add1 (- x height)) (+ x (sub1 height))))))
 (define-values
  (min-y max-y)
  (match
   direction
   ((== right) (values y (+ y width)))
   ((== left) (values (add1 (- y width)) y))
   (else (values (add1 (- y width)) (+ y width)))))
 (values min-x max-x min-y max-y))

(define/ctc-helper
 ((coord-within-box/c start-pos height width direction) cell-coord)
 (match-define (vector start-x start-y) start-pos)
 (match-define (vector cell-x cell-y) cell-coord)
 (define-values
  (min-x max-x min-y max-y)
  (room-bounds start-x start-y height width direction))
 (and (>= cell-x min-x) (<= cell-x max-x) (>= cell-y min-y) (<= cell-y max-y)))

(define/contract
 (try-add-rectangle grid pos height width direction)
 (configurable-ctc
  (max
   (->i
    ((grid grid?)
     (pos (grid) (and/c array-coord? (within-grid/c grid)))
     (height index?)
     (width index?)
     (direction direction?))
    (result
     (pos height width direction)
     (or-#f/c
      (room-with/c
       (=/c height)
       (=/c width)
       (alistof
        (and/c array-coord? (coord-within-box/c pos height width direction))
        cell%/c)
       any/c
       any/c)))))
  (types (-> grid? array-coord? index? index? direction? (or-#f/c any-room?))))
 (match-define (vector x y) pos)
 (define min-x
   (match
    direction
    ((== down) x)
    ((== up) (+ (- x height) 1))
    (else (sub1 (- x (random (- height 2)))))))
 (define min-y
   (match
    direction
    ((== right) y)
    ((== left) (+ (- y width) 1))
    (else (sub1 (- y (random (- width 2)))))))
 (define max-x (+ min-x height))
 (define max-y (+ min-y width))
 (define-values
  (success? poss->cells free-cells extension-points)
  (for*/fold
   ((success? #t) (poss->cells '()) (free-cells '()) (extension-points '()))
   ((x (in-range min-x max-x)) (y (in-range min-y max-y)))
   (cond
    ((not success?) (values success? poss->cells free-cells extension-points))
    (success?
     (define c (and (index? x) (index? y) (grid-ref grid (vector x y))))
     (cond
      ((and c (or (is-a? c void-cell%) (is-a? c wall%)))
       (define p (vector (assert x index?) (assert y index?)))
       (define x-wall? (or (= x min-x) (= x (sub1 max-x))))
       (define y-wall? (or (= y min-y) (= y (sub1 max-y))))
       (if (or x-wall? y-wall?)
         (values
          #t
          (dict-set poss->cells p wall%)
          free-cells
          (if (and x-wall? y-wall?)
            extension-points
            (cons p extension-points)))
         (values
          #t
          (dict-set poss->cells p empty-cell%)
          (cons p free-cells)
          extension-points)))
      (else (values #f '() '() '())))))))
 (and success? (room height width poss->cells free-cells extension-points)))

(define/contract
 (commit-room grid room)
 (configurable-ctc
  (max
   (->i
    ((grid grid?) (room any-room?))
    (result void?)
    #:post
    (grid room)
    (for/and
     ((pos+cell% (in-list (room-poss->cells room))))
     (match-define (cons pos poss-cell%) pos+cell%)
     (is-a? (grid-ref grid pos) poss-cell%))))
  (types (-> grid? any-room? void?)))
 (for
  ((pos+cell% (in-list (room-poss->cells room))))
  (match-define (cons pos cell%) pos+cell%)
  (array-set! grid pos (new cell%))))

(define/contract
 (random-direction)
 (configurable-ctc
  (max (-> (curryr member (list left right up down))))
  (types (-> direction?)))
 (random-from (list left right up down)))

(define/contract
 (horizontal? dir)
 (configurable-ctc
  (max
   (->i
    ((dir direction?))
    (result (dir) (if (member dir (list left right)) #t #f))))
  (types (-> direction? boolean?)))
 (or (eq? dir right) (eq? dir left)))

(define/contract
 (vertical? dir)
 (configurable-ctc
  (max
   (->i
    ((dir direction?))
    (result (dir) (if (member dir (list up down)) #t #f))))
  (types (-> direction? boolean?)))
 (or (eq? dir up) (eq? dir down)))

(define/contract
 (new-room grid pos dir)
 (configurable-ctc
  (max
   (->i
    ((grid grid?)
     (pos (grid) (and/c array-coord? (within-grid/c grid)))
     (dir direction?))
    (result
     (pos dir)
     (or-#f/c
      (room-with/c
       (random-result-between/c 7 11)
       (random-result-between/c 7 11)
       (alistof
        (and/c array-coord? (coord-within-box/c pos 11 11 dir))
        cell%/c)
       any/c
       any/c)))))
  (types (-> grid? array-coord? direction? (or-#f/c any-room?))))
 (define w (assert (random-between 7 11) index?))
 (define h (assert (random-between 7 11) index?))
 (try-add-rectangle grid pos w h dir))

(define/contract
 (new-corridor grid pos dir)
 (configurable-ctc
  (max
   (->i
    ((grid grid?) (pos array-coord?) (dir direction?))
    (result
     (pos dir)
     (let* ((h? (horizontal? dir)) (h (if h? 3 8)) (w (if h? 10 3)))
       (or-#f/c
        (room-with/c
         (random-result-between/c 3 8)
         (random-result-between/c 3 10)
         (alistof
          (and/c array-coord? (coord-within-box/c pos h w dir))
          cell%/c)
         any/c
         any/c))))))
  (types (-> grid? array-coord? direction? (or-#f/c any-room?))))
 (define h? (horizontal? dir))
 (define len
   (assert (if h? (random-between 6 10) (random-between 5 8)) index?))
 (define h (if h? 3 len))
 (define w (if h? len 3))
 (try-add-rectangle grid pos h w dir))

(define/ctc-helper
 (door-count grid)
 (define height (grid-height grid))
 (define width (grid-width grid))
 (for/fold
  ((doors 0))
  ((row-index (in-range height)))
  (+ doors (vector-count (curryr is-a? door%) (vector-ref grid row-index)))))

(define/ctc-helper (room-count grid) (add1 (door-count grid)))

(define/ctc-helper ((room-count>=/c n) grid) (>= (room-count grid) n))

(define/contract
 (generate-dungeon encounters)
 (configurable-ctc
  (max
   (->i
    ((encounters (listof exact-nonnegative-integer?)))
    (result (encounters) (and/c grid? (room-count>=/c (length encounters))))))
  (types (-> (listof exact-nonnegative-integer?) grid?)))
 (define n-rooms (max (length encounters) (random-between 6 9)))
 (define grid
   (build-array
    (vector dungeon-height dungeon-width)
    (lambda _ (new void-cell%))))
 (define first-room
   (let loop ()
     (define starting-point
       (vector
        (assert (random dungeon-height) index?)
        (assert (random dungeon-width) index?)))
     (define first-room (new-room grid starting-point (random-direction)))
     (or first-room (loop))))
 (commit-room grid first-room)
 (when animate-generation? (display (show-grid grid)))
 (define connections '())
 (define (extension-points/room room)
   (for/list ((e (in-list (room-extension-points room)))) (cons e room)))
 (let loop ()
   (define-values
    (n all-rooms _2)
    (for/fold
     ((n-rooms-to-go (sub1 n-rooms))
      (rooms (list first-room))
      (extension-points (extension-points/room first-room)))
     ((i (in-range ITERS)))
     (cond
      ((= n-rooms-to-go 0) (values n-rooms-to-go rooms extension-points))
      (else
       (define (add-room origin-room room ext (corridor #f) (new-ext #f))
         (when corridor (commit-room grid corridor))
         (commit-room grid room)
         (define door-kind
           (if (horizontal? dir) vertical-door% horizontal-door%))
         (array-set! grid ext (new door-kind))
         (when new-ext (array-set! grid new-ext (new door-kind)))
         (set! connections (cons (cons origin-room room) connections))
         (when animate-generation? (display (show-grid grid)))
         (values
          (sub1 n-rooms-to-go)
          (cons room rooms)
          (append
           (if corridor (extension-points/room corridor) '())
           (extension-points/room room)
           extension-points)))
       (match-define `(,ext . ,origin-room) (random-from extension-points))
       (define dir (random-direction))
       (cond
        ((and (zero? (random 4)) (new-room grid ext dir))
         =>
         (lambda (room) (add-room origin-room room ext)))
        ((new-corridor grid ext dir)
         =>
         (lambda (corridor)
           (define new-ext
             (dir
              ext
              (if (horizontal? dir)
                (assert (sub1 (room-width corridor)) index?)
                (assert (sub1 (room-height corridor)) index?))))
           (cond
            ((new-room grid new-ext dir)
             =>
             (lambda (room) (add-room origin-room room ext corridor new-ext)))
            (else (values n-rooms-to-go rooms extension-points)))))
        (else (values n-rooms-to-go rooms extension-points)))))))
   (cond
    ((not (= n 0))
     (set! n-rooms (max (length encounters) (sub1 n-rooms)))
     (loop))
    (else
     (define potential-connections
       (for*/fold
        ((potential-connections '()))
        ((r1 (in-list all-rooms))
         (r2 (in-list all-rooms))
         #:unless
         (or (eq? r1 r2)
             (member (cons r1 r2) connections)
             (member (cons r2 r1) connections)
             (member (cons r2 r1) potential-connections)))
        (cons (cons r1 r2) potential-connections)))
     (for
      ((r1+r2 (in-list potential-connections)))
      (match-define (cons r1 r2) r1+r2)
      (define common
        (set-intersect (room-extension-points r1) (room-extension-points r2)))
      (define possible-doors
        (filter
         (lambda (x) x)
         (for/list
          ((pos (in-list common)))
          (cond
           ((and (counts-as-free? grid (up pos))
                 (counts-as-free? grid (down pos)))
            (cons pos horizontal-door%))
           ((and (counts-as-free? grid (left pos))
                 (counts-as-free? grid (right pos)))
            (cons pos vertical-door%))
           (else #f)))))
      (when (not (empty? possible-doors))
        (match-define (cons pos door-kind) (random-from possible-doors))
        (array-set! grid pos (new door-kind))))
     grid))))

(define/contract
 (counts-as-free? grid pos)
 (configurable-ctc
  (max
   (->i
    ((grid grid?) (pos array-coord?))
    (result boolean?)
    #:post
    (grid pos result)
    (let ((c (grid-ref grid pos)))
      (or (false? result) (is-a? c empty-cell%) (is-a? c door%)))))
  (types (-> grid? array-coord? boolean?)))
 (cond
  ((hash-ref free-cache pos #f) => (lambda (x) x))
  (else
   (define c (grid-ref grid pos))
   (define res (or (is-a? c empty-cell%) (is-a? c door%)))
   (hash-set! free-cache pos res)
   res)))

(define/contract
 (hash-clear! h)
 (configurable-ctc (max any/c) (types any/c))
 (void))

(define/contract
 (smooth-walls grid)
 (configurable-ctc (max (-> grid? grid?)) (types (-> grid? grid?)))
 (for*
  ((x (in-range (grid-height grid))) (y (in-range (grid-width grid))))
  (smooth-single-wall grid (vector (assert x index?) (assert y index?))))
 (set! wall-cache (make-hash))
 (set! free-cache (make-hash))
 grid)

(define/contract
 (smooth-single-wall grid pos)
 (configurable-ctc
  (max (-> grid? array-coord? void?))
  (types (-> grid? array-coord? void?)))
 (define (wall-or-door? pos)
   (cond
    ((hash-ref wall-cache pos #f) => (lambda (x) x))
    (else
     (define c (grid-ref grid pos))
     (define res (or (is-a? c wall%) (is-a? c door%)))
     (hash-set! wall-cache pos res)
     res)))
 (when (is-a? (grid-ref grid pos) wall%)
   (define u (wall-or-door? (up pos)))
   (define d (wall-or-door? (down pos)))
   (define l (wall-or-door? (left pos)))
   (define r (wall-or-door? (right pos)))
   (define fu (delay (counts-as-free? grid (up pos))))
   (define fd (delay (counts-as-free? grid (down pos))))
   (define fl (delay (counts-as-free? grid (left pos))))
   (define fr (delay (counts-as-free? grid (right pos))))
   (define ful (delay (counts-as-free? grid (up (left pos)))))
   (define fur (delay (counts-as-free? grid (up (right pos)))))
   (define fdl (delay (counts-as-free? grid (down (left pos)))))
   (define fdr (delay (counts-as-free? grid (down (right pos)))))
   (define (2-of-3? a b c) (or (and a b #t) (and a c #t) (and b c #t)))
   (array-set!
    grid
    pos
    (new
     (match*
      (u d l r)
      ((#f #f #f #f) pillar%)
      ((#f #f #f #t) horizontal-wall%)
      ((#f #f #t #f) horizontal-wall%)
      ((#f #f #t #t) horizontal-wall%)
      ((#f #t #f #f) vertical-wall%)
      ((#f #t #f #t) north-west-wall%)
      ((#f #t #t #f) north-east-wall%)
      ((#f #t #t #t)
       (cond
        ((2-of-3? (force fu) (force fdl) (force fdr)) north-tee-wall%)
        ((force fu) horizontal-wall%)
        ((force fdl) north-east-wall%)
        ((force fdr) north-west-wall%)
        (else (raise-user-error 'cond))))
      ((#t #f #f #f) vertical-wall%)
      ((#t #f #f #t) south-west-wall%)
      ((#t #f #t #f) south-east-wall%)
      ((#t #f #t #t)
       (cond
        ((2-of-3? (force fd) (force ful) (force fur)) south-tee-wall%)
        ((force fd) horizontal-wall%)
        ((force ful) south-east-wall%)
        ((force fur) south-west-wall%)
        (else (raise-user-error 'cond))))
      ((#t #t #f #f) vertical-wall%)
      ((#t #t #f #t)
       (cond
        ((2-of-3? (force fl) (force fur) (force fdr)) west-tee-wall%)
        ((force fl) vertical-wall%)
        ((force fur) south-west-wall%)
        ((force fdr) north-west-wall%)
        (else (raise-user-error 'cond))))
      ((#t #t #t #f)
       (cond
        ((2-of-3? (force fr) (force ful) (force fdl)) east-tee-wall%)
        ((force fr) vertical-wall%)
        ((force ful) south-east-wall%)
        ((force fdl) north-east-wall%)
        (else (raise-user-error 'nocd))))
      ((#t #t #t #t)
       (cond
        ((or (and (force ful) (force fdr)) (and (force fur) (force fdl)))
         four-corner-wall%)
        ((and (force ful) (force fur)) south-tee-wall%)
        ((and (force fdl) (force fdr)) north-tee-wall%)
        ((and (force ful) (force fdl)) east-tee-wall%)
        ((and (force fur) (force fdr)) west-tee-wall%)
        ((force ful) south-east-wall%)
        ((force fur) south-west-wall%)
        ((force fdl) north-east-wall%)
        ((force fdr) north-west-wall%)
        (else (raise-user-error 'cond))))
      ((_ _ _ _) (raise-user-error 'voidcase)))))))

(define/contract
 LOOPS
 (configurable-ctc
  (max exact-nonnegative-integer?)
  (types exact-nonnegative-integer?))
 1)

(define/contract
 (main)
 (configurable-ctc (max any/c) (types any/c))
 (for
  ((_i (in-range LOOPS)))
  (show-grid (smooth-walls (generate-dungeon (range N))))
  (reset!)))

#;(module+
 test
 (require rackunit)
 (require (only-in "grid.rkt" parse-grid))
 (define (render-grid g) (string-join g "\n" #:after-last "\n"))
 (define (empty-grid) (build-array #(6 6) (lambda _ (new void-cell%))))
 (define g1 (empty-grid))
 (check-equal?
  (show-grid g1)
  (render-grid '("......" "......" "......" "......" "......" "......")))
 (commit-room g1 (or (try-add-rectangle g1 #(2 1) 3 3 right) (error 'commit)))
 (check-equal?
  (show-grid g1)
  (render-grid '("......" ".XXX.." ".X X.." ".XXX.." "......" "......")))
 (check-false (try-add-rectangle g1 #(2 2) 3 3 up))
 (commit-room g1 (or (try-add-rectangle g1 #(3 3) 3 3 down) (error 'commit)))
 (check-equal?
  (show-grid g1)
  (render-grid '("......" ".XXX.." ".X X.." ".XXXX." "..X X." "..XXX.")))
 (check-equal?
  (show-grid (smooth-walls g1))
  (render-grid '("......" ".╔═╗.." ".║ ║.." ".╚╦╩╗." "..║ ║." "..╚═╝.")))
 (define g2 (empty-grid))
 (commit-room g2 (or (try-add-rectangle g2 #(1 1) 3 4 right) (error 'commit)))
 (check-equal?
  (show-grid g2)
  (render-grid '(".XXXX." ".X  X." ".XXXX." "......" "......" "......")))
 (check-equal?
  (show-grid (smooth-walls g2))
  (render-grid '(".╔══╗." ".║  ║." ".╚══╝." "......" "......" "......")))
 (define (walls/one) (parse-grid '("..." ".X." "...")))
 (check-equal?
  (show-grid (smooth-walls (walls/one)))
  (render-grid '("..." ".#." "...")))
 (define (walls/corners) (parse-grid '("...." ".XX." ".XX." "....")))
 (check-equal?
  (show-grid (smooth-walls (walls/corners)))
  (render-grid '("...." ".╔╗." ".╚╝." "....")))
 (define (walls/horiz) (parse-grid '("....." ".XXX." ".....")))
 (check-equal?
  (show-grid (smooth-walls (walls/horiz)))
  (render-grid '("....." ".═══." ".....")))
 (define (walls/vert) (parse-grid '("..." ".X." ".X." ".X." "...")))
 (check-equal?
  (show-grid (smooth-walls (walls/vert)))
  (render-grid '("..." ".║." ".║." ".║." "...")))
 (define (walls/tees-1)
   (parse-grid '("......" ".. X.." ".XXX ." ". XXX." "..X .." "......")))
 (check-equal?
  (show-grid (smooth-walls (walls/tees-1)))
  (render-grid '("......" ".. ║.." ".═╦╣ ." ". ╠╩═." "..║ .." "......")))
 (define (walls/tees-2)
   (parse-grid '("......" "..X .." ". XXX." ".XXX ." ".. X.." "......")))
 (check-equal?
  (show-grid (smooth-walls (walls/tees-2)))
  (render-grid '("......" "..║ .." ". ╠╦═." ".═╩╣ ." ".. ║.." "......")))
 (define (walls/no-tees-1)
   (parse-grid '("......" "...X.." ".XXX ." ". XXX." "..X..." "......")))
 (check-equal?
  (show-grid (smooth-walls (walls/no-tees-1)))
  (render-grid '("......" "...║.." ".═╗║ ." ". ║╚═." "..║..." "......")))
 (define (walls/no-tees-2)
   (parse-grid '("......" ".. X.." ".XXX.." "..XXX." "..X .." "......")))
 (check-equal?
  (show-grid (smooth-walls (walls/no-tees-2)))
  (render-grid '("......" ".. ║.." ".══╝.." "..╔══." "..║ .." "......")))
 (define (walls/no-tees-3)
   (parse-grid '("......" "..X..." ". XXX." ".XXX ." "...X.." "......")))
 (check-equal?
  (show-grid (smooth-walls (walls/no-tees-3)))
  (render-grid '("......" "..║..." ". ║╔═." ".═╝║ ." "...║.." "......")))
 (define (walls/no-tees-4)
   (parse-grid '("......" "..X .." "..XXX." ".XXX.." ".. X.." "......")))
 (check-equal?
  (show-grid (smooth-walls (walls/no-tees-4)))
  (render-grid '("......" "..║ .." "..╚══." ".══╗.." ".. ║.." "......")))
 (define (walls/no-tees-err)
   (parse-grid '("......" "...X.." ".XXX.." "..XXX." "..X..." "......")))
 (check-exn #rx"cond" (thunk (show-grid (smooth-walls (walls/no-tees-err)))))
 (define (walls/plus-1)
   (parse-grid '("....." ". X ." ".XXX." ". X ." ".....")))
 (check-equal?
  (show-grid (smooth-walls (walls/plus-1)))
  (render-grid '("....." ". ║ ." ".═╬═." ". ║ ." ".....")))
 (define (walls/plus-2)
   (parse-grid '("....." ". X.." ".XXX." "..X ." ".....")))
 (check-equal?
  (show-grid (smooth-walls (walls/plus-2)))
  (render-grid '("....." ". ║.." ".═╬═." "..║ ." ".....")))
 (define (walls/plus-3)
   (parse-grid '("....." "..X ." ".XXX." ". X.." ".....")))
 (check-equal?
  (show-grid (smooth-walls (walls/plus-3)))
  (render-grid '("....." "..║ ." ".═╬═." ". ║.." ".....")))
 (define (walls/no-plus-1)
   (parse-grid
    '("......." ". X.X ." ".XXXXX." "..X.X.." ".XXXXX." ". X.X ." ".......")))
 (check-equal?
  (show-grid (smooth-walls (walls/no-plus-1)))
  (render-grid
   '("......." ". ║.║ ." ".═╝═╚═." "..║.║.." ".═╗═╔═." ". ║.║ ." ".......")))
 (define (walls/no-plus-2)
   (parse-grid '("....." ". X ." ".XXX." "..X.." ".XXX." ". X ." ".....")))
 (check-equal?
  (show-grid (smooth-walls (walls/no-plus-2)))
  (render-grid '("....." ". ║ ." ".═╩═." "..║.." ".═╦═." ". ║ ." ".....")))
 (define (walls/no-plus-3)
   (parse-grid '("......." ". X.X ." ".XXXXX." ". X.X ." ".......")))
 (check-equal?
  (show-grid (smooth-walls (walls/no-plus-3)))
  (render-grid '("......." ". ║.║ ." ".═╣═╠═." ". ║.║ ." ".......")))
 (define (walls/some-doors)
   (parse-grid
    '("......XXXX."
      ".XXX..X  X."
      ".| XXXX-XX."
      ".X      X.."
      ".X-XXX-XX.."
      ".....X X..."
      ".....XXX..."
      "...........")))
 (check-equal?
  (show-grid (smooth-walls (walls/some-doors)))
  (render-grid
   '("......╔══╗."
     ".╔═╗..║  ║."
     "._ ╚══╩'╦╝."
     ".║      ║.."
     ".╚'══╦'╦╝.."
     ".....║ ║..."
     ".....╚═╝..."
     "..........."))))

