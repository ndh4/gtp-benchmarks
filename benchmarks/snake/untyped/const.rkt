#lang racket

(require (only-in
          "data.rkt"
          posn
          snake
          world
          posn?
          posn-type?
          posn/c
          snake-segs?
          snake-segs=?/c
          snake-dir?
          snake-type?
          snake/c
          snake=?/c
          posn=?/c
          world/c
          world-type?
          world=?/c
          food=?/c)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require (only-in "data.rkt" posn=?))

(provide GRID-SIZE
         BOARD-HEIGHT
         BOARD-WIDTH
         BOARD-HEIGHT-PIXELS
         BOARD-WIDTH-PIXELS
         SEGMENT-RADIUS
         FOOD-RADIUS
         WORLD)

(define/contract
 GRID-SIZE
 (configurable-ctc (max (and/c natural? (=/c 30))) (types natural?))
 30)

(define/contract
 BOARD-HEIGHT
 (configurable-ctc (max (and/c natural? (=/c 20))) (types natural?))
 20)

(define/contract
 BOARD-WIDTH
 (configurable-ctc (max (and/c natural? (=/c 30))) (types natural?))
 30)

(define/contract
 (BOARD-HEIGHT-PIXELS)
 (configurable-ctc
  (max (-> (and/c natural? (=/c (* GRID-SIZE BOARD-HEIGHT)))))
  (types (-> natural?)))
 (* GRID-SIZE BOARD-HEIGHT))

(define/contract
 (BOARD-WIDTH-PIXELS)
 (configurable-ctc
  (max (-> (and/c natural? (=/c (* GRID-SIZE BOARD-WIDTH)))))
  (types (-> natural?)))
 (* GRID-SIZE BOARD-WIDTH))

(define/contract
 (SEGMENT-RADIUS)
 (configurable-ctc (max (-> (=/c (/ GRID-SIZE 2)))) (types (-> number?)))
 (/ GRID-SIZE 2))

(define/contract
 (FOOD-RADIUS)
 (configurable-ctc (max (-> (=/c (/ GRID-SIZE 2)))) (types (-> number?)))
 (SEGMENT-RADIUS))

(define/contract
 (WORLD)
 (configurable-ctc
  (max
   (->
    (world/c
     (snake/c "right" (snake-segs=?/c (list (posn 5 3))))
     (posn/c 8 12))))
  (types (-> world-type?)))
 (world (snake "right" (cons (posn 5 3) empty)) (posn 8 12)))

