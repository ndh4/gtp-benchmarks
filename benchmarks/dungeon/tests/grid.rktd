#hash((0 . #s(target-file "./benchmarks/dungeon/original/grid.rkt" ()))
      (1 . #s(context 0 (begin (require "cell.rkt") (require rackunit)) ()))
      (2 . #s(test 1 (check-equal? (left (vector 3 3)) (vector 3 2)) ()))
      (3 . #s(test 1 (check-equal? (left (vector 3 0)) (vector 3 0)) ()))
      (4 . #s(test 1 (check-equal? (left (vector 3 3) 2) (vector 3 1)) ()))
      (5 . #s(test 1 (check-equal? (left (vector 3 3) 4) (vector 3 0)) ()))
      (6 . #s(test 1 (check-equal? (right (vector 3 3)) (vector 3 4)) ()))
      (7 . #s(test 1 (check-equal? (right (vector 3 3) 5) (vector 3 8)) ()))
      (8 . #s(test 1 (check-equal? (up (vector 3 3)) (vector 2 3)) ()))
      (9 . #s(test 1 (check-equal? (up (vector 0 3)) (vector 0 3)) ()))
      (10 . #s(test 1 (check-equal? (up (vector 3 3) 2) (vector 1 3)) ()))
      (11 . #s(test 1 (check-equal? (up (vector 3 3) 4) (vector 0 3)) ()))
      (12 . #s(test 1 (check-equal? (down (vector 3 3)) (vector 4 3)) ()))
      (13 . #s(test 1 (check-equal? (down (vector 3 3) 5) (vector 8 3)) ()))
      (14
       .
       #s(context
          1
          (begin
            (define (parse-and-show los) (show-grid (parse-grid los)))
            (define (render-grid g) (string-join g "\n" #:after-last "\n"))
            (define g1 '(" ")))
          ()))
      (15 . #s(test 14 (check-equal? (parse-and-show g1) " \n") ()))
      (16
       .
       #s(context
          14
          (begin
            (define g2
              '("XXXXX'XXXX"
                "X. #     X"
                "_    *  #X"
                "X        X"
                "XXXXXXXXXX")))
          ()))
      (17 . #s(test 16 (check-equal? (parse-and-show g2) (render-grid g2)) ()))
      (18 . #s(context 16 (begin (define g2* (parse-grid g2))) ()))
      (19 . #s(test 18 (check-true (within-grid? g2* '#(0 0))) ()))
      (20 . #s(test 18 (check-true (within-grid? g2* '#(0 1))) ()))
      (21 . #s(test 18 (check-true (within-grid? g2* '#(1 0))) ()))
      (22 . #s(test 18 (check-true (within-grid? g2* '#(4 4))) ()))
      (23 . #s(test 18 (check-false (within-grid? g2* '#(0 10))) ()))
      (24 . #s(test 18 (check-false (within-grid? g2* '#(5 0))) ()))
      (25 . #s(test 18 (check-false (within-grid? g2* '#(5 10))) ()))
      (26 . #s(test 18 (check-equal? (grid-ref g2* '#(0 0)) (new wall%)) ()))
      (27
       .
       #s(test
          18
          (check-equal? (grid-ref g2* '#(2 0)) (new other-vertical-door%))
          ()))
      (28
       .
       #s(test
          18
          (check-equal? (grid-ref g2* '#(0 5)) (new other-horizontal-door%))
          ()))
      (29 . #s(test 18 (check-equal? (grid-ref g2* '#(1 3)) (new pillar%)) ()))
      (30
       .
       #s(test 18 (check-equal? (grid-ref g2* '#(3 1)) (new empty-cell%)) ()))
      (31 . #s(test 18 (check-equal? (grid-ref g2* '#(2 5)) (new cell%)) ()))
      (32
       .
       #s(test 18 (check-equal? (grid-ref g2* '#(1 1)) (new void-cell%)) ()))
      (33 . #s(test 18 (check-equal? (grid-width g2*) 10) ()))
      (34 . #s(test 18 (check-equal? (grid-height g2*) 5) ()))
      (35
       .
       #s(context
          18
          (begin
            (define g3
              '("XXXXX-XXXX"
                "X. +     X"
                "|    *  +X"
                "X        X"
                "XXXXXXXXXX")))
          ()))
      (36 . #s(test 35 (check-equal? (parse-and-show g3) (render-grid g2)) ()))
      (37 . #s(context 35 (begin (define g3* (parse-grid g3))) ()))
      (38
       .
       #s(test
          37
          (check-equal? (grid-ref g3* '#(2 0)) (new vertical-door%))
          ()))
      (39
       .
       #s(test
          37
          (check-equal? (grid-ref g3* '#(0 5)) (new horizontal-door%))
          ())))
