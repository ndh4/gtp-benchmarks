#lang racket

(require (except-in "data.rkt" posn=?)
         (except-in
          "const.rkt"
          WORLD
          FOOD-RADIUS
          SEGMENT-RADIUS
          BOARD-WIDTH-PIXELS
          BOARD-HEIGHT-PIXELS
          BOARD-WIDTH
          BOARD-HEIGHT
          GRID-SIZE)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require (only-in
          "const.rkt"
          WORLD
          FOOD-RADIUS
          SEGMENT-RADIUS
          BOARD-WIDTH-PIXELS
          BOARD-HEIGHT-PIXELS
          BOARD-WIDTH
          BOARD-HEIGHT
          GRID-SIZE))

(require (only-in "data.rkt" posn=?))

(provide snake-wall-collide?
         head-collide?
         snake-self-collide?
         segs-self-collide?)

(define/contract
 (snake-wall-collide? snk)
 (configurable-ctc
  (max
   (->i
    ((snk snake-type?))
    (result
     (snk)
     (match snk ((snake _ (cons h _)) (head-collide? h)) (_ #f)))))
  (types (-> snake-type? boolean?)))
 (head-collide? (car (snake-segs snk))))

(define/contract
 (head-collide? p)
 (configurable-ctc
  (max
   (->i
    ((p posn-type?))
    (result
     (p)
     (not (and (< 0 (posn-x p) BOARD-WIDTH) (< 0 (posn-y p) BOARD-HEIGHT))))))
  (types (-> posn-type? boolean?)))
 (or (<= (posn-x p) 0)
     (>= (posn-x p) BOARD-WIDTH)
     (<= (posn-y p) 0)
     (>= (posn-y p) BOARD-HEIGHT)))

(define/ctc-helper (truthy->bool x) (if x #t #f))

(define/ctc-helper memf? (compose truthy->bool memf))

(define/contract
 (snake-self-collide? snk)
 (configurable-ctc
  (max
   (->i
    ((snk snake-type?))
    (result
     (snk)
     (match snk ((snake _ (cons h t)) (memf? (posn=?/c h) t)) (_ #f)))))
  (types (-> snake-type? boolean?)))
 (segs-self-collide? (car (snake-segs snk)) (cdr (snake-segs snk))))

(define/contract
 (segs-self-collide? h segs)
 (configurable-ctc
  (max
   (->i
    ((h posn-type?) (segs snake-segs?))
    (result (h segs) (if (empty? segs) #f (memf? (posn=?/c h) segs)))))
  (types (-> posn-type? snake-segs? boolean?)))
 (cond
  ((empty? segs) #f)
  (else (or (posn=? (car segs) h) (segs-self-collide? h (cdr segs))))))

#;(module+
 test
 (require rackunit)
 (check-false (head-collide? (posn (/ BOARD-WIDTH 2) (/ BOARD-HEIGHT 2))))
 (check-false (head-collide? (posn 1 1)))
 (check-true (head-collide? (posn 1 0)))
 (check-true (head-collide? (posn -1 1)))
 (check-false (head-collide? (posn (- BOARD-WIDTH 1) 1)))
 (check-true (head-collide? (posn BOARD-WIDTH 1)))
 (check-true (head-collide? (posn (- BOARD-WIDTH 2) 0)))
 (check-false (head-collide? (posn 1 (- BOARD-HEIGHT 1))))
 (check-true (head-collide? (posn 1 BOARD-HEIGHT)))
 (check-true (head-collide? (posn 0 (- BOARD-HEIGHT 1))))
 (check-false (head-collide? (posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))
 (check-true (head-collide? (posn BOARD-WIDTH BOARD-HEIGHT)))
 (check-true (head-collide? (posn (- BOARD-WIDTH 1) BOARD-HEIGHT)))
 (check-true (head-collide? (posn BOARD-WIDTH (- BOARD-HEIGHT 1)))))

