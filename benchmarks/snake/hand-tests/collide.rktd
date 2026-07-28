#hash((0 . #s(target-file "./benchmarks/snake/original/collide.rkt" ()))
      (1 . #s(context 0 (begin (require rackunit)) ()))
      (2
       .
       #s(test
          1
          (check-false
           (head-collide? (posn (/ BOARD-WIDTH 2) (/ BOARD-HEIGHT 2))))
          ()))
      (3 . #s(test 1 (check-false (head-collide? (posn 1 1))) ()))
      (4 . #s(test 1 (check-true (head-collide? (posn 1 0))) ()))
      (5 . #s(test 1 (check-true (head-collide? (posn -1 1))) ()))
      (6
       .
       #s(test 1 (check-false (head-collide? (posn (- BOARD-WIDTH 1) 1))) ()))
      (7 . #s(test 1 (check-true (head-collide? (posn BOARD-WIDTH 1))) ()))
      (8
       .
       #s(test 1 (check-true (head-collide? (posn (- BOARD-WIDTH 2) 0))) ()))
      (9
       .
       #s(test 1 (check-false (head-collide? (posn 1 (- BOARD-HEIGHT 1)))) ()))
      (10 . #s(test 1 (check-true (head-collide? (posn 1 BOARD-HEIGHT))) ()))
      (11
       .
       #s(test 1 (check-true (head-collide? (posn 0 (- BOARD-HEIGHT 1)))) ()))
      (12
       .
       #s(test
          1
          (check-false
           (head-collide? (posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))
          ()))
      (13
       .
       #s(test
          1
          (check-true (head-collide? (posn BOARD-WIDTH BOARD-HEIGHT)))
          ()))
      (14
       .
       #s(test
          1
          (check-true (head-collide? (posn (- BOARD-WIDTH 1) BOARD-HEIGHT)))
          ()))
      (15
       .
       #s(test
          1
          (check-true (head-collide? (posn BOARD-WIDTH (- BOARD-HEIGHT 1))))
          ())))
