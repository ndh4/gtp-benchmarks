#hash((0 . #s(target-file "./benchmarks/dungeon/original/main.rkt" ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (require (only-in "grid.rkt" parse-grid))
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
       #s(test
          6
          (check-equal?
           (show-grid (smooth-walls g1))
           (render-grid
            '("......" ".╔═╗.." ".║ ║.." ".╚╦╩╗." "..║ ║." "..╚═╝.")))
          ()))
      (9
       .
       #s(context
          6
          (begin
            (define g2 (empty-grid))
            (commit-room
             g2
             (or (try-add-rectangle g2 #(1 1) 3 4 right) (error 'commit))))
          ()))
      (10
       .
       #s(test
          9
          (check-equal?
           (show-grid g2)
           (render-grid
            '(".XXXX." ".X  X." ".XXXX." "......" "......" "......")))
          ()))
      (11
       .
       #s(test
          9
          (check-equal?
           (show-grid (smooth-walls g2))
           (render-grid
            '(".╔══╗." ".║  ║." ".╚══╝." "......" "......" "......")))
          ()))
      (12
       .
       #s(context
          9
          (begin (define (walls/one) (parse-grid '("..." ".X." "..."))))
          ()))
      (13
       .
       #s(test
          12
          (check-equal?
           (show-grid (smooth-walls (walls/one)))
           (render-grid '("..." ".#." "...")))
          ()))
      (14
       .
       #s(context
          12
          (begin
            (define (walls/corners)
              (parse-grid '("...." ".XX." ".XX." "...."))))
          ()))
      (15
       .
       #s(test
          14
          (check-equal?
           (show-grid (smooth-walls (walls/corners)))
           (render-grid '("...." ".╔╗." ".╚╝." "....")))
          ()))
      (16
       .
       #s(context
          14
          (begin
            (define (walls/horiz) (parse-grid '("....." ".XXX." "....."))))
          ()))
      (17
       .
       #s(test
          16
          (check-equal?
           (show-grid (smooth-walls (walls/horiz)))
           (render-grid '("....." ".═══." ".....")))
          ()))
      (18
       .
       #s(context
          16
          (begin
            (define (walls/vert)
              (parse-grid '("..." ".X." ".X." ".X." "..."))))
          ()))
      (19
       .
       #s(test
          18
          (check-equal?
           (show-grid (smooth-walls (walls/vert)))
           (render-grid '("..." ".║." ".║." ".║." "...")))
          ()))
      (20
       .
       #s(context
          18
          (begin
            (define (walls/tees-1)
              (parse-grid
               '("......" ".. X.." ".XXX ." ". XXX." "..X .." "......"))))
          ()))
      (21
       .
       #s(test
          20
          (check-equal?
           (show-grid (smooth-walls (walls/tees-1)))
           (render-grid
            '("......" ".. ║.." ".═╦╣ ." ". ╠╩═." "..║ .." "......")))
          ()))
      (22
       .
       #s(context
          20
          (begin
            (define (walls/tees-2)
              (parse-grid
               '("......" "..X .." ". XXX." ".XXX ." ".. X.." "......"))))
          ()))
      (23
       .
       #s(test
          22
          (check-equal?
           (show-grid (smooth-walls (walls/tees-2)))
           (render-grid
            '("......" "..║ .." ". ╠╦═." ".═╩╣ ." ".. ║.." "......")))
          ()))
      (24
       .
       #s(context
          22
          (begin
            (define (walls/no-tees-1)
              (parse-grid
               '("......" "...X.." ".XXX ." ". XXX." "..X..." "......"))))
          ()))
      (25
       .
       #s(test
          24
          (check-equal?
           (show-grid (smooth-walls (walls/no-tees-1)))
           (render-grid
            '("......" "...║.." ".═╗║ ." ". ║╚═." "..║..." "......")))
          ()))
      (26
       .
       #s(context
          24
          (begin
            (define (walls/no-tees-2)
              (parse-grid
               '("......" ".. X.." ".XXX.." "..XXX." "..X .." "......"))))
          ()))
      (27
       .
       #s(test
          26
          (check-equal?
           (show-grid (smooth-walls (walls/no-tees-2)))
           (render-grid
            '("......" ".. ║.." ".══╝.." "..╔══." "..║ .." "......")))
          ()))
      (28
       .
       #s(context
          26
          (begin
            (define (walls/no-tees-3)
              (parse-grid
               '("......" "..X..." ". XXX." ".XXX ." "...X.." "......"))))
          ()))
      (29
       .
       #s(test
          28
          (check-equal?
           (show-grid (smooth-walls (walls/no-tees-3)))
           (render-grid
            '("......" "..║..." ". ║╔═." ".═╝║ ." "...║.." "......")))
          ()))
      (30
       .
       #s(context
          28
          (begin
            (define (walls/no-tees-4)
              (parse-grid
               '("......" "..X .." "..XXX." ".XXX.." ".. X.." "......"))))
          ()))
      (31
       .
       #s(test
          30
          (check-equal?
           (show-grid (smooth-walls (walls/no-tees-4)))
           (render-grid
            '("......" "..║ .." "..╚══." ".══╗.." ".. ║.." "......")))
          ()))
      (32
       .
       #s(context
          30
          (begin
            (define (walls/no-tees-err)
              (parse-grid
               '("......" "...X.." ".XXX.." "..XXX." "..X..." "......"))))
          ()))
      (33
       .
       #s(test
          32
          (check-exn
           #rx"cond"
           (thunk (show-grid (smooth-walls (walls/no-tees-err)))))
          ()))
      (34
       .
       #s(context
          32
          (begin
            (define (walls/plus-1)
              (parse-grid '("....." ". X ." ".XXX." ". X ." "....."))))
          ()))
      (35
       .
       #s(test
          34
          (check-equal?
           (show-grid (smooth-walls (walls/plus-1)))
           (render-grid '("....." ". ║ ." ".═╬═." ". ║ ." ".....")))
          ()))
      (36
       .
       #s(context
          34
          (begin
            (define (walls/plus-2)
              (parse-grid '("....." ". X.." ".XXX." "..X ." "....."))))
          ()))
      (37
       .
       #s(test
          36
          (check-equal?
           (show-grid (smooth-walls (walls/plus-2)))
           (render-grid '("....." ". ║.." ".═╬═." "..║ ." ".....")))
          ()))
      (38
       .
       #s(context
          36
          (begin
            (define (walls/plus-3)
              (parse-grid '("....." "..X ." ".XXX." ". X.." "....."))))
          ()))
      (39
       .
       #s(test
          38
          (check-equal?
           (show-grid (smooth-walls (walls/plus-3)))
           (render-grid '("....." "..║ ." ".═╬═." ". ║.." ".....")))
          ()))
      (40
       .
       #s(context
          38
          (begin
            (define (walls/no-plus-1)
              (parse-grid
               '("......."
                 ". X.X ."
                 ".XXXXX."
                 "..X.X.."
                 ".XXXXX."
                 ". X.X ."
                 "......."))))
          ()))
      (41
       .
       #s(test
          40
          (check-equal?
           (show-grid (smooth-walls (walls/no-plus-1)))
           (render-grid
            '("......."
              ". ║.║ ."
              ".═╝═╚═."
              "..║.║.."
              ".═╗═╔═."
              ". ║.║ ."
              ".......")))
          ()))
      (42
       .
       #s(context
          40
          (begin
            (define (walls/no-plus-2)
              (parse-grid
               '("....." ". X ." ".XXX." "..X.." ".XXX." ". X ." "....."))))
          ()))
      (43
       .
       #s(test
          42
          (check-equal?
           (show-grid (smooth-walls (walls/no-plus-2)))
           (render-grid
            '("....." ". ║ ." ".═╩═." "..║.." ".═╦═." ". ║ ." ".....")))
          ()))
      (44
       .
       #s(context
          42
          (begin
            (define (walls/no-plus-3)
              (parse-grid
               '("......." ". X.X ." ".XXXXX." ". X.X ." "......."))))
          ()))
      (45
       .
       #s(test
          44
          (check-equal?
           (show-grid (smooth-walls (walls/no-plus-3)))
           (render-grid '("......." ". ║.║ ." ".═╣═╠═." ". ║.║ ." ".......")))
          ()))
      (46
       .
       #s(context
          44
          (begin
            (define (walls/some-doors)
              (parse-grid
               '("......XXXX."
                 ".XXX..X  X."
                 ".| XXXX-XX."
                 ".X      X.."
                 ".X-XXX-XX.."
                 ".....X X..."
                 ".....XXX..."
                 "..........."))))
          ()))
      (47
       .
       #s(test
          46
          (check-equal?
           (show-grid (smooth-walls (walls/some-doors)))
           (render-grid
            '("......╔══╗."
              ".╔═╗..║  ║."
              "._ ╚══╩'╦╝."
              ".║      ║.."
              ".╚'══╦'╦╝.."
              ".....║ ║..."
              ".....╚═╝..."
              "...........")))
          ())))
