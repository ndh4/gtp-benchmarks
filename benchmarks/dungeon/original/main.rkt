#lang racket

(require racket/class
         "../base/un-types.rkt"
         racket/match
         racket/contract
         (only-in
          "../../../ctcs/common.rkt"
          make-dependent-contract
          or-#f/c
          exact-nonnegative-integer?/tiny-gen)
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
          direction?
          generate-pos-within-grid))

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

(require (only-in "utils.rkt" reset! random random-from random-between))

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
         smooth-walls
         smooth-single-wall
         LOOPS
         main)

(struct
 unique-alist-contract
 (key-ctc val-ctc)
 #:property
 prop:contract
 (build-contract-property
  #:name
  (λ (c) 'alistof-no-duplicates)
  #:first-order
  (λ (c)
    (λ (al)
      (define base
        (listof
         (cons/c
          (unique-alist-contract-key-ctc c)
          (unique-alist-contract-val-ctc c))))
      (and (contract-first-order-passes? base al)
           (not (check-duplicates (map car al))))))
  #:late-neg-projection
  (λ (c)
    (λ (blame)
      (λ (val neg-blame)
        (define base
          (listof
           (cons/c
            (unique-alist-contract-key-ctc c)
            (unique-alist-contract-val-ctc c))))
        (define base-proj (contract-late-neg-projection base))
        (define projected-val ((base-proj blame) val neg-blame))
        (if (not (check-duplicates (map car projected-val)))
          projected-val
          (raise-blame-error blame val '(expected "no duplicate keys"))))))
  #:generate
  (λ (c)
    (λ (fuel)
      (define base
        (listof
         (cons/c
          (unique-alist-contract-key-ctc c)
          (unique-alist-contract-val-ctc c))))
      (define raw-gen (contract-random-generate/choose base fuel))
      (λ ()
        (remove-duplicates
         (raw-gen)
         (lambda (a b) (equal? (car a) (car b)))))))))

(define (alistof key/c val/c)
  (unique-alist-contract
   (coerce-contract 'key/c key/c)
   (coerce-contract 'val/c val/c)))

(struct room (height width poss->cells free-cells extension-points) #:mutable)

(define/ctc-helper
 (room-with/c height/c width/c poss->cells/c free-cells/c extension-points/c)
 (struct/c
  room
  (and/c index/c height/c)
  (and/c index/c width/c)
  (and/c (alistof array-coord? cell%/c) poss->cells/c)
  (and/c (listof array-coord?) free-cells/c)
  (and/c (listof array-coord?) extension-points/c)))

(define/ctc-helper any-room? (room-with/c any/c any/c any/c any/c any/c))

(define (generate-room-for-grid grid fuel)
  (define gh (grid-height grid))
  (define gw (grid-width grid))
  (define r-height (if (> gh 1) (+ 1 (random (sub1 gh))) 1))
  (define r-width (if (> gw 1) (+ 1 (random (sub1 gw))) 1))
  (define absolute-coords
    (for*/list
     ((r (in-range 0 r-height)) (c (in-range 0 r-width)))
     (vector r c)))
  (define raw-alist
    (for/list
     ((pos (in-list absolute-coords)) #:when (zero? (random 2)))
     (cons pos cell%)))
  (define unique-poss->cells
    (remove-duplicates raw-alist (lambda (a b) (equal? (car a) (car b)))))
  (room
   r-height
   r-width
   unique-poss->cells
   (take absolute-coords (random (add1 (length absolute-coords))))
   (if (null? absolute-coords) null (list (first absolute-coords)))))

(define/ctc-helper
 commit-room-args/c*
 (make-dependent-contract
  grid?
  (lambda (grid)
    (room-with/c
     (</c (grid-height grid))
     (</c (grid-width grid))
     (alistof (within-grid/c grid) cell%/c)
     (listof (within-grid/c grid))
     (listof (within-grid/c grid))))
  generate-room-for-grid))

(define/ctc-helper commit-room-grid-ctc (first commit-room-args/c*))

(define/ctc-helper commit-room-room-ctc (second commit-room-args/c*))

(define/ctc-helper commit-room-ctc-reset!? (third commit-room-args/c*))

(define/ctc-helper
 try-add-rectangle-args/c*
 (make-dependent-contract
  grid?
  (lambda (grid) (and/c array-coord? (within-grid/c grid)))
  generate-pos-within-grid))

(define/ctc-helper
 try-add-rectangle-grid-ctc
 (first try-add-rectangle-args/c*))

(define/ctc-helper
 try-add-rectangle-pos-ctc
 (second try-add-rectangle-args/c*))

(define/ctc-helper
 try-add-rectangle-ctc-reset!?
 (third try-add-rectangle-args/c*))

(define/ctc-helper
 new-corridor-args/c*
 (make-dependent-contract
  grid?
  (lambda (grid) (and/c array-coord? (within-grid/c grid)))
  generate-pos-within-grid))

(define/ctc-helper new-corridor-grid-ctc (first new-corridor-args/c*))

(define/ctc-helper new-corridor-pos-ctc (second new-corridor-args/c*))

(define/ctc-helper new-corridor-ctc-reset!? (third new-corridor-args/c*))

(define/ctc-helper
 new-room-args/c*
 (make-dependent-contract
  grid?
  (lambda (grid) (and/c array-coord? (within-grid/c grid)))
  generate-pos-within-grid))

(define/ctc-helper new-room-grid-ctc (first new-room-args/c*))

(define/ctc-helper new-room-pos-ctc (second new-room-args/c*))

(define/ctc-helper new-room-ctc-reset!? (third new-room-args/c*))

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

(define/ctc-helper
 (valid-grid-for-cache/c)
 (flat-named-contract
  `(valid-grid-for-cache/c ,free-cache)
  (lambda (grid)
    (and (contract-first-order-passes? grid? grid)
         (for/and
          (((pos result) (in-hash free-cache)))
          (define r (vector-ref pos 0))
          (define c-idx (vector-ref pos 1))
          (or (false? result)
              (and (< r (vector-length grid))
                   (< c-idx (vector-length (vector-ref grid 0)))
                   (let ((cell (vector-ref (vector-ref grid r) c-idx)))
                     (or (is-a? cell empty-cell%) (is-a? cell door%))))))))
  (lambda (fuel)
    (define random-cell (contract-random-generate/choose cell%? fuel))
    (lambda ()
      (define-values
       (max-r max-c)
       (for/fold
        ((mr 0) (mc 0))
        (((pos result) (in-hash free-cache)))
        (if result
          (values (max mr (vector-ref pos 0)) (max mc (vector-ref pos 1)))
          (values mr mc))))
      (define rows (+ max-r 1 (random 3)))
      (define cols (+ max-c 1 (random 3)))
      (define grid
        (build-vector
         rows
         (lambda (_) (build-vector cols (lambda (_) (random-cell))))))
      (for
       (((pos result) (in-hash free-cache)))
       (define r (vector-ref pos 0))
       (define c-idx (vector-ref pos 1))
       (when (and (< r rows) (< c-idx cols))
         (define row-vec (vector-ref grid r))
         (if result
           (if (zero? (random 2))
             (vector-set! row-vec c-idx (new empty-cell%))
             (vector-set! row-vec c-idx (new door%)))
           (vector-set! row-vec c-idx (random-cell)))))
      grid))))

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
    ((grid try-add-rectangle-grid-ctc)
     (pos try-add-rectangle-pos-ctc)
     (height size>2/c)
     (width size>2/c)
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
       any/c)))
    #:post
    ()
    (try-add-rectangle-ctc-reset!?)))
  (types
   (-> grid? array-coord? index/c index/c direction? (or-#f/c any-room?))))
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
    ((grid commit-room-grid-ctc) (room commit-room-room-ctc))
    (result void?)
    #:post
    (grid room)
    (and (commit-room-ctc-reset!?)
         (for/and
          ((pos+cell% (in-list (room-poss->cells room))))
          (match-define (cons pos poss-cell%) pos+cell%)
          (is-a? (grid-ref grid pos) poss-cell%)))))
  (types (-> grid? any-room? void?)))
 (for
  ((pos+cell% (in-list (room-poss->cells room))))
  (match-define (cons pos cell%) pos+cell%)
  (array-set! grid pos (new cell%))))

(define/contract
 (random-direction)
 (configurable-ctc (max (-> direction?)) (types (-> direction?)))
 (random-from (list left right up down)))

(define/contract
 (horizontal? dir)
 (configurable-ctc
  (max (-> direction? boolean?))
  (types (-> direction? boolean?)))
 (or (eq? dir right) (eq? dir left)))

(define/contract
 (vertical? dir)
 (configurable-ctc
  (max (-> direction? boolean?))
  (types (-> direction? boolean?)))
 (or (eq? dir up) (eq? dir down)))

(define/contract
 (new-room grid pos dir)
 (configurable-ctc
  (max
   (->i
    ((grid new-room-grid-ctc) (pos new-room-pos-ctc) (dir direction?))
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
    ((grid new-corridor-grid-ctc) (pos new-corridor-pos-ctc) (dir direction?))
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
    ((encounters (listof exact-nonnegative-integer?/tiny-gen)))
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
    ((grid (valid-grid-for-cache/c)) (pos array-coord?))
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
  (max (-> (valid-grid-for-cache/c) array-coord? void?))
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
        (else north-tee-wall%)))
      ((#t #f #f #f) vertical-wall%)
      ((#t #f #f #t) south-west-wall%)
      ((#t #f #t #f) south-east-wall%)
      ((#t #f #t #t)
       (cond
        ((2-of-3? (force fd) (force ful) (force fur)) south-tee-wall%)
        ((force fd) horizontal-wall%)
        ((force ful) south-east-wall%)
        ((force fur) south-west-wall%)
        (else south-tee-wall%)))
      ((#t #t #f #f) vertical-wall%)
      ((#t #t #f #t)
       (cond
        ((2-of-3? (force fl) (force fur) (force fdr)) west-tee-wall%)
        ((force fl) vertical-wall%)
        ((force fur) south-west-wall%)
        ((force fdr) north-west-wall%)
        (else west-tee-wall%)))
      ((#t #t #t #f)
       (cond
        ((2-of-3? (force fr) (force ful) (force fdl)) east-tee-wall%)
        ((force fr) vertical-wall%)
        ((force ful) south-east-wall%)
        ((force fdl) north-east-wall%)
        (else east-tee-wall%)))
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
        (else pillar%)))
      ((_ _ _ _) (raise-user-error 'voidcase)))))))

(define/contract
 LOOPS
 (configurable-ctc
  (max exact-nonnegative-integer?)
  (types exact-nonnegative-integer?))
 1)

(define/contract
 (main)
 (configurable-ctc (max (-> void)) (types (-> void)))
 (for
  ((_i (in-range LOOPS)))
  (show-grid (smooth-walls (generate-dungeon (range N))))
  (reset!)))

(module+
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

