#hash((0 . #s(target-file "./benchmarks/dungeon/original/utils.rkt" ()))
      (1
       .
       #s(context
          0
          (begin (require rackunit) (set-box! r* '(1 2 3 4 5)) (reset!))
          ()))
      (2 . #s(test 1 (check-equal? (unbox r*) orig) ()))
      (3
       .
       #s(context
          1
          (begin
            (set-box! r* '(1 2 3 4 5))
            (define-values
             (r1 r2 r3 r4 r5)
             (values
              (random 3)
              (random 123)
              (random 1)
              (random 3)
              (random 3))))
          ()))
      (4 . #s(test 3 (check-equal? r1 1) ()))
      (5 . #s(test 3 (check-equal? r2 2) ()))
      (6 . #s(test 3 (check-equal? r3 0) ()))
      (7 . #s(test 3 (check-equal? r4 1) ()))
      (8 . #s(test 3 (check-equal? r5 2) ()))
      (9
       .
       #s(context
          3
          (begin
            (set-box! r* '(1 2 3 4 5))
            (define-values
             (rb1 rb2 rb3 rb4 rb5)
             (values
              (random-between 4 8)
              (random-between 9 11)
              (random-between 3 9)
              (random-between 2 5)
              (random-between 2 5))))
          ()))
      (10 . #s(test 9 (check-equal? rb1 5) ()))
      (11 . #s(test 9 (check-equal? rb2 9) ()))
      (12 . #s(test 9 (check-equal? rb3 6) ()))
      (13 . #s(test 9 (check-equal? rb4 3) ()))
      (14 . #s(test 9 (check-equal? rb5 4) ()))
      (15
       .
       #s(context
          9
          (begin
            (set-box! r* '(15 20 25))
            (define-values (d61 d62 d63) (values (d6) (d6) (d6))))
          ()))
      (16 . #s(test 15 (check-equal? d61 4) ()))
      (17 . #s(test 15 (check-equal? d62 3) ()))
      (18 . #s(test 15 (check-equal? d63 2) ()))
      (19
       .
       #s(context
          15
          (begin
            (set-box! r* '(15 20 25))
            (define-values (d201 d202 d203) (values (d20) (d20) (d20))))
          ()))
      (20 . #s(test 19 (check-equal? d201 16) ()))
      (21 . #s(test 19 (check-equal? d202 1) ()))
      (22 . #s(test 19 (check-equal? d203 6) ()))
      (23 . #s(test 19 (check-equal? (article #t #t) "The") ()))
      (24 . #s(test 19 (check-equal? (article #t #f) "A") ()))
      (25 . #s(test 19 (check-equal? (article #f #t) "the") ()))
      (26 . #s(test 19 (check-equal? (article #f #f) "a") ()))
      (27 . #s(test 19 (check-equal? (article #t #t #:an? #t) "The") ()))
      (28 . #s(test 19 (check-equal? (article #t #f #:an? #t) "An") ()))
      (29 . #s(test 19 (check-equal? (article #f #t #:an? #t) "the") ()))
      (30 . #s(test 19 (check-equal? (article #f #f #:an? #t) "an") ())))
