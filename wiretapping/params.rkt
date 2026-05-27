#lang racket/base

(provide path-to-bench-dirs
         benchmarks
         contract-level
         MEMORY_LIMIT)

(define path-to-bench-dirs
  "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks")

(define benchmarks
  '(
    "mbta"
    "morsecode"
    "sieve"
    "snake"
    "kcfa"
    "dungeon"
    "forth"
    )
  )

(define contract-level 'types)

(define MEMORY_LIMIT 50)