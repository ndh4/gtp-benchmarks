#hash((0 . #s(target-file "./benchmarks/dungeon/original/main.rkt" ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define (render-grid g) (string-join g "\n" #:after-last "\n"))
            (define (empty-grid)
              (build-array #(6 6) (lambda _ (new void-cell%))))
            (define g1 (empty-grid)))
          ()))
      (2
       .
       #s(test
          1
          (check-equal?
           (show-grid g1)
           (render-grid
            '("......" "......" "......" "......" "......" "......")))
          ()))
      (3
       .
       #s(context
          1
          (begin
            (commit-room
             g1
             (or (try-add-rectangle g1 #(2 1) 3 3 right) (error 'commit))))
          ()))
      (4
       .
       #s(test
          3
          (check-equal?
           (show-grid g1)
           (render-grid
            '("......" ".XXX.." ".X X.." ".XXX.." "......" "......")))
          ()))
      (5 . #s(test 3 (check-false (try-add-rectangle g1 #(2 2) 3 3 up)) ()))
      (6
       .
       #s(context
          3
          (begin
            (commit-room
             g1
             (or (try-add-rectangle g1 #(3 3) 3 3 down) (error 'commit))))
          ()))
      (7
       .
       #s(test
          6
          (check-equal?
           (show-grid g1)
           (render-grid
            '("......" ".XXX.." ".X X.." ".XXXX." "..X X." "..XXX.")))
          ()))
      (8
       .
       #s(context
          6
          (begin
            (define g2 (empty-grid))
            (commit-room
             g2
             (or (try-add-rectangle g2 #(1 1) 3 4 right) (error 'commit))))
          ()))
      (9
       .
       #s(test
          8
          (check-equal?
           (show-grid g2)
           (render-grid
            '(".XXXX." ".X  X." ".XXXX." "......" "......" "......")))
          ())))
