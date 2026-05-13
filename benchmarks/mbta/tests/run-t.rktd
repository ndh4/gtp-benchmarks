#hash((0 . #s(target-file "./benchmarks/mbta/original/run-t.rkt" ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define (path-len p) (length (string-split p "\n"))))
          ()))
      (2
       .
       #s(test
          1
          (check-equal? (path-len (run-t "from Airport to Northeastern")) 14)
          ()))
      (3 . #s(context 1 (begin (define r1 (run-t "disable Government"))) ()))
      (4 . #s(test 3 (check-equal? (path-len r1) 1) ()))
      (5
       .
       #s(test
          3
          (check-equal? (path-len (run-t "from Airport to Northeastern")) 16)
          ()))
      (6 . #s(context 3 (begin (define r2 (run-t "enable Government"))) ()))
      (7 . #s(test 6 (check-equal? (path-len r2) 1) ()))
      (8
       .
       #s(test
          6
          (check-equal? (path-len (run-t "from Airport to Harvard Square")) 12)
          ()))
      (9 . #s(context 6 (begin (define r3 (run-t "disable Park Street"))) ()))
      (10 . #s(test 9 (check-equal? (path-len r3) 1) ()))
      (11
       .
       #s(test
          9
          (check-true
           (string-prefix?
            (run-t "from Northeastern to Harvard Square")
            "it is currently impossible"))
          ()))
      (12 . #s(context 9 (begin (define r4 (run-t "enable Park Street"))) ()))
      (13 . #s(test 12 (check-equal? (path-len r4) 1) ()))
      (14
       .
       #s(test
          12
          (check-equal?
           (path-len (run-t "from Northeastern to Harvard Square"))
           12)
          ()))
      (15
       .
       #s(test 12 (check-equal? (run-t "blabla") "message not understood") ()))
      (16
       .
       #s(test
          12
          (check-equal?
           (run-t "from abcd abcd to abcdeee")
           "no such station: abcd abcd")
          ()))
      (17
       .
       #s(test
          12
          (check-equal?
           (run-t "from blabla to Northeastern")
           "no such station: blabla")
          ()))
      (18
       .
       #s(test
          12
          (check-true
           (string-prefix?
            (run-t "from N to Northeastern")
            "disambiguate your current location"))
          ()))
      (19
       .
       #s(test
          12
          (check-true
           (string-prefix?
            (run-t "from Northeastern to N")
            "disambiguate your destination"))
          ()))
      (20
       .
       #s(test
          12
          (check-true
           (string-prefix?
            (run-t "from N to G")
            "disambiguate your current location"))
          ()))
      (21
       .
       #s(test
          12
          (check-true
           (string-prefix?
            (run-t "from N to N")
            "disambiguate your current location"))
          ()))
      (22
       .
       #s(test
          12
          (check-equal?
           (run-t "disable blabla")
           "no such station to disable: blabla")
          ()))
      (23
       .
       #s(test
          12
          (check-true
           (string-prefix?
            (run-t "from Northeastern to Northeastern")
            "Close your eyes"))
          ()))
      (24
       .
       #s(test
          12
          (check-true
           (string-prefix?
            (run-t "from Northeaster to Northeastern")
            "Close your eyes"))
          ())))
