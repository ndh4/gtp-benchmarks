#hash((0
       .
       #s(target-file
          "/home/breitnw/Documents/research/parent-cat/gtp-benchmarks/benchmarks/snake/original/cut-tail.rkt"
          ()))
      (1 . #s(context 0 (begin (require rackunit)) ()))
      (2 . #s(test 1 (check-equal? (cut-tail (list (posn 1 1))) (list)) ()))
      (3
       .
       #s(test
          1
          (check-equal?
           (cut-tail (list (posn 1 1) (posn 1 2)))
           (list (posn 1 1)))
          ()))
      (4
       .
       #s(test
          1
          (check-equal?
           (cut-tail (list (posn 1 1) (posn 1 2) (posn 1 3) (posn 2 3)))
           (list (posn 1 1) (posn 1 2) (posn 1 3)))
          ()))
      (5
       .
       #s(test
          1
          (check-equal?
           (cut-tail (list (posn 2 1) (posn 3 4) (posn 2 3)))
           (list (posn 2 1) (posn 3 4)))
          ())))
