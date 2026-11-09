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
         (except-in "motion-help.rkt" snake-grow snake-slither next-head)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require (only-in "motion-help.rkt" snake-grow snake-slither next-head))

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

(provide r
         reset!
         world->world
         eating?
         snake-change-direction
         world-change-dir
         snake-eat)

(define/contract
 r
 (configurable-ctc
  (max pseudo-random-generator?)
  (types pseudo-random-generator?))
 (make-pseudo-random-generator))

(define/contract
 (reset!)
 (configurable-ctc
  (max
   (->*
    ()
    void?
    #:post
    (equal?
     (pseudo-random-generator->vector r)
     (let ((r* (make-pseudo-random-generator)))
       (parameterize
        ((current-pseudo-random-generator r*))
        (random-seed 1324)
        (pseudo-random-generator->vector r*))))))
  (types (-> void?)))
 (parameterize ((current-pseudo-random-generator r)) (random-seed 1324)))

(define/contract
 (world->world w)
 (configurable-ctc
  (max
   (->i
    ((w world-type?))
    (result
     (w)
     (match
      w
      ((world (snake dir (cons h t)) food)
       #:when
       (posn=? h food)
       (world/c (snake/c dir (compose (=/c (+ (length t) 2)) length)) any/c))
      ((world (snake dir segs) food)
       (world/c
        (snake/c dir (compose (=/c (length segs)) length))
        (food=?/c food)))))))
  (types (-> world-type? world-type?)))
 (cond
  ((eating? w) (snake-eat w))
  (else (world (snake-slither (world-snake w)) (world-food w)))))

(define/contract
 (eating? w)
 (configurable-ctc
  (max
   (->i
    ((w world-type?))
    (result
     (w)
     (match
      w
      ((world (snake _ (cons p1 _)) p2) #:when (posn=? p1 p2) #t)
      (_ #f)))))
  (types (-> world-type? boolean?)))
 (posn=? (world-food w) (car (snake-segs (world-snake w)))))

(define/contract
 (snake-change-direction snk dir)
 (configurable-ctc
  (max
   (->i
    ((snk snake-type?) (dir snake-dir?))
    (result (snk dir) (snake/c dir (snake-segs=?/c (snake-segs snk))))))
  (types (-> snake-type? string? snake-type?)))
 (snake dir (snake-segs snk)))

(define/contract
 (world-change-dir w dir)
 (configurable-ctc
  (max
   (->i
    ((w world-type?) (dir snake-dir?))
    (result
     (w dir)
     (match
      w
      ((world (snake _ segs) food)
       (world/c (snake/c dir (snake-segs=?/c segs)) (food=?/c food)))))))
  (types (-> world-type? string? world-type?)))
 (world (snake-change-direction (world-snake w) dir) (world-food w)))

(define/contract
 (snake-eat w)
 (configurable-ctc
  (max
   (->i
    ((w world-type?))
    (result
     (w)
     (world/c
      (snake=?/c (snake-grow (world-snake w)))
      (posn/c
       (integer-in 0 (sub1 BOARD-WIDTH))
       (integer-in 0 (sub1 BOARD-HEIGHT)))))))
  (types (-> world-type? world-type?)))
 (define i (add1 (random (sub1 BOARD-WIDTH) r)))
 (define j (add1 (random (sub1 BOARD-HEIGHT) r)))
 (world (snake-grow (world-snake w)) (posn i j)))

