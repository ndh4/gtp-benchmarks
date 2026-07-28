#hash((0 . #s(target-file "./benchmarks/mbta/original/main.rkt" ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define (run-query str)
              (define r (run-t str))
              (if r
                r
                (error
                 'main
                 (format "run-t failed to respond to query ~e\n" str))))
            (define (num-pieces res) (length (string-split res "\n"))))
          ()))
      (2
       .
       #s(test
          1
          (check-equal?
           (num-pieces (run-query (path "Airport" "Northeastern")))
           14)
          ()))
      (3
       .
       #s(context
          1
          (begin (define res1 (run-query (disable "Government"))))
          ()))
      (4 . #s(test 3 (check-equal? (num-pieces res1) 1) ()))
      (5
       .
       #s(test
          3
          (check-equal?
           (num-pieces (run-query (path "Airport" "Northeastern")))
           16)
          ()))
      (6
       .
       #s(context
          3
          (begin (define res2 (run-query (enable "Government"))))
          ()))
      (7 . #s(test 6 (check-equal? (num-pieces res2) 1) ()))
      (8
       .
       #s(test
          6
          (check-equal?
           (num-pieces (run-query (path "Airport" "Harvard Square")))
           12)
          ()))
      (9
       .
       #s(context
          6
          (begin (define res3 (run-query (disable "Park Street"))))
          ()))
      (10 . #s(test 9 (check-equal? (num-pieces res3) 1) ()))
      (11
       .
       #s(test
          9
          (check-equal?
           (num-pieces (run-query (path "Northeastern" "Harvard Square")))
           1)
          ()))
      (12
       .
       #s(context
          9
          (begin (define res4 (run-query (enable "Park Street"))))
          ()))
      (13 . #s(test 12 (check-equal? (num-pieces res4) 1) ()))
      (14
       .
       #s(test
          12
          (check-equal?
           (num-pieces (run-query (path "Northeastern" "Harvard Square")))
           12)
          ())))
