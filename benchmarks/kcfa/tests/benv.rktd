#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Documents/Research-Cloud/teco-parent/contracts-and-testing/bex/orchestration/../../../gtp-benchmarks/benchmarks/kcfa/original/benv.rkt"
          ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit (only-in racket/format ~a))
            (define benv1 (benv-extend empty-benv 'x (Binding 'x '(g123)))))
          ()))
      (2
       .
       #s(test
          1
          (check-equal? (benv-lookup benv1 'x) (Binding 'x '(g123)))
          ()))
      (3
       .
       #s(context
          1
          (begin
            (define benv2
              (benv-extend*
               benv1
               '(x k)
               (list (Binding 'x '(g19)) (Binding 'k '(g19))))))
          ()))
      (4
       .
       #s(test 3 (check-equal? (benv-lookup benv2 'k) (Binding 'k '(g19))) ()))
      (5
       .
       #s(test
          3
          (check-equal? (benv-lookup benv2 'x) (Binding 'x '(g19)))
          ())))
