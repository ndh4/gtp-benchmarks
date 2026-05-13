#lang racket
(define-syntax ctc-level 'none)
(require "../ctcs/configurable.rkt")

(provide/configurable-contract
          [GRID-SIZE ([max natural?])]
          [BOARD-HEIGHT ([max natural?])]
          [BOARD-HEIGHT-PIXELS
           ([max (-> (and/c natural? (=/c (* GRID-SIZE BOARD-HEIGHT))))])])

(define GRID-SIZE #f)

(define BOARD-HEIGHT 1)

(define
  (BOARD-HEIGHT-PIXELS)

  (* GRID-SIZE BOARD-HEIGHT))
