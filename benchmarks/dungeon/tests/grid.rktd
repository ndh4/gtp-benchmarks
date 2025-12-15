#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Research-Local/blgt-parent/gtp-benchmarks/benchmarks/dungeon/original/grid.rkt"
          ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define (parse-and-show los) (show-grid (parse-grid los)))
            (define (render-grid g) (string-join g "\n" #:after-last "\n"))
            (define g1 '(" ")))
          ()))
      (2 . #s(test 1 (check-equal? (parse-and-show g1) " \n") ()))
      (3
       .
       #s(context
          1
          (begin
            (define g2
              '("**********"
                "*        *"
                "*        *"
                "*        *"
                "**********")))
          ()))
      (4 . #s(test 3 (check-equal? (parse-and-show g2) (render-grid g2)) ()))
      (5 . #s(context 3 (begin (define g2* (parse-grid g2))) ()))
      (6 . #s(test 5 (check-true (within-grid? g2* '#(0 0))) ()))
      (7 . #s(test 5 (check-true (within-grid? g2* '#(0 1))) ()))
      (8 . #s(test 5 (check-true (within-grid? g2* '#(1 0))) ()))
      (9 . #s(test 5 (check-true (within-grid? g2* '#(4 4))) ()))
      (10 . #s(test 5 (check-false (within-grid? g2* '#(0 10))) ()))
      (11 . #s(test 5 (check-false (within-grid? g2* '#(5 0))) ()))
      (12 . #s(test 5 (check-false (within-grid? g2* '#(5 10))) ())))
