#lang racket
(define-syntax ctc-level 'none)
(require "../ctcs/precision-config.rkt")

(provide GRID-SIZE BOARD-HEIGHT BOARD-HEIGHT-PIXELS)

(define/contract GRID-SIZE (configurable-ctc [max natural?]) #f)

(define/contract BOARD-HEIGHT (configurable-ctc [max natural?]) 1)

(define/contract
  (BOARD-HEIGHT-PIXELS)
  (configurable-ctc [max (-> (and/c natural? (=/c (* GRID-SIZE BOARD-HEIGHT))))])

  (* GRID-SIZE BOARD-HEIGHT))