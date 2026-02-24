#lang racket

(require (except-in "data.rkt" posn=?)
         (except-in "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")
(require/configurable-contract "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE )
(require/configurable-contract "data.rkt" posn=? )

(provide/configurable-contract
 [snake-wall-collide? ([max (->i ([snk snake-type?])
                                 [result (snk)
                                         (match snk
                                           [(snake _ (cons h _)) (head-collide? h)]
                                           [_ #f])])]
                       [types (snake-type? . -> . boolean?)])]
 [head-collide? ([max (->i ([p posn-type?])
                           [result (p)
                                   (not (and (< 0 (posn-x p) BOARD-WIDTH)
                                             (< 0 (posn-y p) BOARD-HEIGHT)))])]
                 [types (posn-type? . -> . boolean?)])]
 [snake-self-collide? ([max (->i ([snk snake-type?])
                                 [result (snk)
                                         (match snk
                                           [(snake _ (cons h t))
                                            (memf? (posn=?/c h) t)]
                                           [_ #f])])]
                       [types (snake-type? . -> . boolean?)])]
 [segs-self-collide? ([max (->i ([h posn-type?]
                                 [segs snake-segs?])
                                [result (h segs)
                                        (if (empty? segs)
                                            #f
                                            (memf? (posn=?/c h) segs))])]
                      [types (posn-type? snake-segs? . -> . boolean?)])])

;; (provide
 ;; snake-wall-collide?
 ;; snake-self-collide?)


;; snake-wall-collide? : Snake -> Boolean
;; Is the snake colliding with any of the walls?
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))

;; head-collide? : Posn -> Boolean
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

(define/ctc-helper (truthy->bool x) (if x #t #f))
(define/ctc-helper memf? (compose truthy->bool memf))

;; snake-self-collide? : Snake -> Boolean
(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

;; segs-self-collide? : Posn Segs -> Boolean
(define (segs-self-collide? h segs)
  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))

(module+ test
  (require rackunit)
  ;; tests for `head-collide?`
  (check-false (head-collide? (posn (/ BOARD-WIDTH 2) (/ BOARD-HEIGHT 2))))
  ;; top left
  (check-false (head-collide? (posn 1 1)))
  (check-true (head-collide? (posn 1 0)))
  (check-true (head-collide? (posn -1 1)))
  ;; top right
  (check-false (head-collide? (posn (- BOARD-WIDTH 1) 1)))
  (check-true (head-collide? (posn BOARD-WIDTH 1)))
  (check-true (head-collide? (posn (- BOARD-WIDTH 2) 0)))
  ;; bottom left
  (check-false (head-collide? (posn 1 (- BOARD-HEIGHT 1))))
  (check-true (head-collide? (posn 1 BOARD-HEIGHT)))
  (check-true (head-collide? (posn 0 (- BOARD-HEIGHT 1))))
  ;; bottom right
  (check-false (head-collide? (posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))
  (check-true (head-collide? (posn BOARD-WIDTH BOARD-HEIGHT)))
  (check-true (head-collide? (posn (- BOARD-WIDTH 1) BOARD-HEIGHT)))
  (check-true (head-collide? (posn BOARD-WIDTH (- BOARD-HEIGHT 1))))

  ;; tests for `snake-wall-collide?`
  )
