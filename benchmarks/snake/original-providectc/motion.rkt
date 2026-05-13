#lang racket

(require (except-in "data.rkt" posn=?)
         (except-in "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE)
         (except-in "motion-help.rkt" snake-grow snake-slither next-head)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")
(require/configurable-contract "motion-help.rkt" snake-grow snake-slither next-head )
(require/configurable-contract "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE )
(require/configurable-contract "data.rkt" posn=? )

(provide/configurable-contract
 [r ([max pseudo-random-generator?]
     [types pseudo-random-generator?])]
 [reset! ([max (->* ()
                    void?
                    #:post
                    (equal?
                     (pseudo-random-generator->vector r)
                     (let ([r* (make-pseudo-random-generator)])
                       (parameterize ([current-pseudo-random-generator r*])
                         (random-seed 1324)
                         (pseudo-random-generator->vector r*)))))]
          [types (-> void?)])]
 [world->world ([max (->i ([w world-type?])
                          [result
                           (w)
                           (match w
                             [(world (snake dir (cons h t)) food) #:when (posn=? h food)
                                                                  (world/c (snake/c dir
                                                                                    (compose (=/c (+ (length t) 2))
                                                                                             length))
                                                                           any/c)]
                             [(world (snake dir segs) food)
                              (world/c (snake/c dir
                                                (compose (=/c (length segs))
                                                         length))
                                       (food=?/c food))])])]
                [types (world-type? . -> . world-type?)])]
 [eating? ([max (->i ([w world-type?])
                     [result (w)
                             (match w
                               [(world (snake _ (cons p1 _)) p2) #:when (posn=? p1 p2)
                                                                 #t]
                               [_ #f])])]
           [types (world-type? . -> . boolean?)])]
 [snake-change-direction ([max (->i ([snk snake-type?]
                                     [dir snake-dir?])
                                    [result (snk dir)
                                            (snake/c dir (snake-segs=?/c (snake-segs snk)))])]
                          [types (snake-type? string? . -> . snake-type?)])]
 [world-change-dir ([max (->i ([w world-type?]
                               [dir snake-dir?])
                              [result (w dir)
                                      (match w
                                        [(world (snake _ segs) food)
                                         (world/c (snake/c dir (snake-segs=?/c segs))
                                                  (food=?/c food))])])]
                    [types (world-type? string? . -> . world-type?)])]
 [snake-eat ([max (->i ([w world-type?])
                       [result (w)
                               (world/c (snake=?/c (snake-grow (world-snake w)))
                                        (posn/c (integer-in 0 (sub1 BOARD-WIDTH))
                                                (integer-in 0 (sub1 BOARD-HEIGHT))))])]
             [types (world-type? . -> . world-type?)])])


;; (provide reset!)
;; (provide
;;  world-change-dir
;;  world->world)


(define r (make-pseudo-random-generator))
(define (reset!)
  (parameterize ((current-pseudo-random-generator r))
    (random-seed 1324)))

;; world->world : World -> World
(define (world->world w)
  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))
;; eating? : World -> Boolean
;; Is the snake eating the food in the world.
(define (eating? w)
  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))
;; snake-change-direction : Snake Direction -> Snake
;; Change the direction of the snake.
(define (snake-change-direction snk dir)
  (snake dir
         (snake-segs snk)))
;; world-change-dir : World Direction -> World
;; Change direction of the world.
(define (world-change-dir w dir)
  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))



;; snake-eat : World -> World
;; Eat the food and generate a new one.
(define (snake-eat w)
  (define i (add1 (random (sub1 BOARD-WIDTH) r)))
  (define j (add1 (random (sub1 BOARD-HEIGHT) r)))
  (world (snake-grow (world-snake w))
         (posn i j)
         
         #;(posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))
