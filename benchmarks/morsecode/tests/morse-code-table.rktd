#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks/morsecode/original/morse-code-table.rkt"
          ()))
      (1 . #s(context 0 (begin (require rackunit)) ()))
      (2 . #s(test 1 (check-equal? (clean-pattern "·.·.·.·") ".......") ()))
      (3 . #s(test 1 (check-equal? (clean-pattern ".......") ".......") ()))
      (4 . #s(test 1 (check-equal? (clean-pattern "––––––") "------") ()))
      (5 . #s(test 1 (check-equal? (clean-pattern "-–––") "----") ()))
      (6 . #s(test 1 (check-equal? (clean-pattern "·––––––·") ".------.") ()))
      (7
       .
       #s(test
          1
          (check-equal? (clean-pattern "abc&nbsp;de&nbsp;fg") "abcdefg")
          ())))
