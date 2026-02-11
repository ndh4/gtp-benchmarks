#lang racket


(require ;; "data.rkt"
 (only-in "data.rkt"
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
 (except-in "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE)
 (except-in "handlers.rkt" game-over? handle-key)
 (except-in "motion.rkt" snake-eat world-change-dir snake-change-direction eating? world->world reset! r)
 "../../../ctcs/configurable.rkt"
 "../../../ctcs/precision-config.rkt"
 "../../../ctcs/common.rkt")
(require/configurable-contract "motion.rkt" snake-eat world-change-dir snake-change-direction eating? world->world reset! r )
(require/configurable-contract "handlers.rkt" game-over? handle-key )
(require/configurable-contract "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE )
(require/configurable-contract "data.rkt" posn=? )

;; (provide/configurable-contract
;;  [replay ([max (world-type?
;;                 (listof (or/c (list/c 'on-key string?)
;;                               (list/c 'on-tick)
;;                               (list/c 'stop-when)))
;;                 . -> .
;;                 void?)]
;;           [types (world-type? (listof (listof (or/c symbol? string?))) . -> . void?)])]
;;  [DATA any/c]
;;  [LOOPS ([max (=/c 1)]
;;          [types natural?])]
;;  [main ([max ((listof (or/c (list/c 'on-key string?)
;;                             (list/c 'on-tick)
;;                             (list/c 'stop-when)))
;;               . -> .
;;               void?)]
;;         [types ((listof (listof (or/c symbol? string?))) . -> . void?)])])

(define (replay w0 hist)
  (reset!)
  (for/fold ([w w0])
            ([cmd (in-list hist)])
    (match cmd
      [`(on-key ,(? string? ke))
       (handle-key w ke)]
      [`(on-tick)
       (world->world w)]
      [`(stop-when)
       (game-over? w)
       w]))
  (void))

(define DATA
  (with-input-from-file "../base/snake-hist.rktd" read))
(define LOOPS
  1)

(define (main hist)
  (define w0 (WORLD))
  (cond [(list? hist)
         (for ((_i (in-range LOOPS)))
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main DATA))
