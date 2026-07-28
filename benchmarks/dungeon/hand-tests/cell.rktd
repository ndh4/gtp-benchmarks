#hash((0 . #s(target-file "./benchmarks/dungeon/original/cell.rkt" ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define player%
              (class object% (define/public (show) #\@) (super-new)))
            (define base-c (new cell%)))
          ()))
      (2 . #s(test 1 (check-equal? (send base-c free?) #f) ()))
      (3 . #s(test 1 (check-equal? (send base-c show) #\*) ()))
      (4 . #s(context 1 (begin (send base-c open)) ()))
      (5
       .
       #s(test
          4
          (check-equal? (car (unbox message-queue)) "Can't open that.")
          ()))
      (6 . #s(context 4 (begin (send base-c close)) ()))
      (7
       .
       #s(test
          6
          (check-equal? (car (unbox message-queue)) "Can't close that.")
          ()))
      (8
       .
       #s(context
          6
          (begin
            (define empty-c (new empty-cell%))
            (define empty-c/player (new empty-cell% (occupant (new player%)))))
          ()))
      (9 . #s(test 8 (check-equal? (send empty-c free?) #t) ()))
      (10 . #s(test 8 (check-equal? (send empty-c/player free?) #f) ()))
      (11 . #s(test 8 (check-equal? (send empty-c show) #\space) ()))
      (12 . #s(test 8 (check-equal? (send empty-c/player show) #\@) ()))
      (13 . #s(context 8 (begin (send empty-c/player open)) ()))
      (14
       .
       #s(test
          13
          (check-equal? (car (unbox message-queue)) "Can't open that.")
          ()))
      (15 . #s(context 13 (begin (send empty-c/player close)) ()))
      (16
       .
       #s(test
          15
          (check-equal? (car (unbox message-queue)) "Can't close that.")
          ()))
      (17 . #s(context 15 (begin (define void-c (new void-cell%))) ()))
      (18 . #s(test 17 (check-equal? (send void-c free?) #f) ()))
      (19 . #s(test 17 (check-equal? (send void-c show) #\.) ()))
      (20 . #s(context 17 (begin (send void-c open)) ()))
      (21
       .
       #s(test
          20
          (check-equal? (car (unbox message-queue)) "Can't open that.")
          ()))
      (22 . #s(context 20 (begin (send void-c close)) ()))
      (23
       .
       #s(test
          22
          (check-equal? (car (unbox message-queue)) "Can't close that.")
          ()))
      (24 . #s(context 22 (begin (define wall-c (new wall%))) ()))
      (25 . #s(test 24 (check-equal? (send wall-c free?) #f) ()))
      (26 . #s(test 24 (check-equal? (send wall-c show) #\X) ()))
      (27 . #s(context 24 (begin (send wall-c open)) ()))
      (28
       .
       #s(test
          27
          (check-equal? (car (unbox message-queue)) "Can't open that.")
          ()))
      (29 . #s(context 27 (begin (send wall-c close)) ()))
      (30
       .
       #s(test
          29
          (check-equal? (car (unbox message-queue)) "Can't close that.")
          ()))
      (31
       .
       #s(context 29 (begin (define vertical-wall-c (new vertical-wall%))) ()))
      (32 . #s(test 31 (check-equal? (send vertical-wall-c free?) #f) ()))
      (33 . #s(test 31 (check-equal? (send vertical-wall-c show) #\║) ()))
      (34 . #s(context 31 (begin (send vertical-wall-c open)) ()))
      (35
       .
       #s(test
          34
          (check-equal? (car (unbox message-queue)) "Can't open that.")
          ()))
      (36 . #s(context 34 (begin (send vertical-wall-c close)) ()))
      (37
       .
       #s(test
          36
          (check-equal? (car (unbox message-queue)) "Can't close that.")
          ()))
      (38
       .
       #s(context
          36
          (begin
            (define door-c (new door%))
            (define door-c/player (new door% (occupant (new player%)))))
          ()))
      (39 . #s(test 38 (check-equal? (send door-c free?) #t) ()))
      (40 . #s(test 38 (check-equal? (send door-c/player free?) #f) ()))
      (41 . #s(test 38 (check-equal? (send door-c show) #\*) ()))
      (42 . #s(test 38 (check-equal? (send door-c/player show) #\*) ()))
      (43 . #s(context 38 (begin (send door-c open)) ()))
      (44
       .
       #s(test
          43
          (check-equal?
           (car (unbox message-queue))
           "The door is already open.")
          ()))
      (45 . #s(context 43 (begin (send door-c close)) ()))
      (46
       .
       #s(test
          45
          (check-equal?
           (car (unbox message-queue))
           "The door is already open.")
          ()))
      (47
       .
       #s(context
          45
          (begin
            (define vertical-door-c (new vertical-door%))
            (define vertical-door-c/player
              (new vertical-door% (occupant (new player%)))))
          ()))
      (48 . #s(test 47 (check-equal? (send vertical-door-c show) #\_) ()))
      (49
       .
       #s(test 47 (check-equal? (send vertical-door-c/player show) #\@) ()))
      (50
       .
       #s(context
          47
          (begin
            (define horizontal-door-c (new horizontal-door%))
            (define horizontal-door-c/player
              (new horizontal-door% (occupant (new player%)))))
          ()))
      (51 . #s(test 50 (check-equal? (send horizontal-door-c show) #\') ()))
      (52
       .
       #s(test 50 (check-equal? (send horizontal-door-c/player show) #\@) ())))
