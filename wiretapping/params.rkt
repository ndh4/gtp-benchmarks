#lang racket/base

(provide path-to-bench-dirs
         benchmarks
         contract-levels
         TIME_LIMIT_SECONDS
         MEMORY_LIMIT_MB)

(define path-to-bench-dirs
  "/Users/nhejduk/Research-Local/teco-parent/gtp-benchmarks/benchmarks")

(define benchmarks
  '(
;    "mbta"
;    "morsecode"
;    "sieve"
;    "snake"
;    "kcfa"
;    "dungeon"
;    "forth"
;    "abm_test"
    "xmanage"
    )
  )

(define contract-levels '(max #;types))

(define TIME_LIMIT_SECONDS 300)
(define MEMORY_LIMIT_MB 6000)