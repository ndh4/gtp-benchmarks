#lang racket

(require (only-in
          "data.rkt"
          posn
          snake
          world
          world-snake
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

(require (only-in
          "collide.rkt"
          segs-self-collide?
          snake-self-collide?
          head-collide?
          snake-wall-collide?))

(require (only-in
          "motion.rkt"
          snake-eat
          world-change-dir
          snake-change-direction
          eating?
          world->world
          reset!
          r))

(require (only-in "data.rkt" posn=?))

(provide handle-key game-over?)

(define/contract
 (handle-key w ke)
 (configurable-ctc
  (max
   (->i
    ((w world-type?) (ke string?))
    (result
     (w ke)
     (world=?/c
      (let ((keymap (hash "w" "up" "s" "down" "a" "left" "d" "right")))
        (match*
         (ke w)
         (((? (curry hash-has-key? keymap)) (world (snake _ segs) food))
          (world (snake (hash-ref keymap ke) segs) food))
         ((_ w) w)))))))
  (types (-> world-type? string? world-type?)))
 (cond
  ((equal? ke "w") (world-change-dir w "up"))
  ((equal? ke "s") (world-change-dir w "down"))
  ((equal? ke "a") (world-change-dir w "left"))
  ((equal? ke "d") (world-change-dir w "right"))
  (else w)))

(define/contract
 (game-over? w)
 (configurable-ctc
  (max
   (->i
    ((w world-type?))
    (result
     (w)
     (match
      w
      ((world s _) (or (snake-wall-collide? s) (snake-self-collide? s)))))))
  (types (-> world-type? boolean?)))
 (or (snake-wall-collide? (world-snake w))
     (snake-self-collide? (world-snake w))))

