#hash((0
       .
       #s(target-file
          "/home/breitnw/Documents/research/parent-cat/gtp-benchmarks/benchmarks/mbta/original/run-t.rkt"
          ()))
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
      (3
       .
       #s(test 1 (check-equal? (path-len (run-t "disable Government")) 1) ()))
      (4
       .
       #s(test
          1
          (check-equal? (path-len (run-t "from Airport to Northeastern")) 16)
          ()))
      (5
       .
       #s(test 1 (check-equal? (path-len (run-t "enable Government")) 1) ()))
      (6
       .
       #s(test
          1
          (check-equal? (path-len (run-t "from Airport to Harvard Square")) 12)
          ()))
      (7
       .
       #s(test 1 (check-equal? (path-len (run-t "disable Park Street")) 1) ()))
      (8
       .
       #s(test
          1
          (check-true
           (string-prefix?
            (run-t "from Northeastern to Harvard Square")
            "it is currently impossible"))
          ()))
      (9
       .
       #s(test 1 (check-equal? (path-len (run-t "enable Park Street")) 1) ()))
      (10
       .
       #s(test
          1
          (check-equal?
           (path-len (run-t "from Northeastern to Harvard Square"))
           12)
          ()))
      (11
       .
       #s(test 1 (check-equal? (run-t "blabla") "message not understood") ()))
      (12
       .
       #s(test
          1
          (check-equal?
           (run-t "from abcd abcd to abcdeee")
           "no such station: abcd abcd")
          ()))
      (13
       .
       #s(test
          1
          (check-equal?
           (run-t "from blabla to Northeastern")
           "no such station: blabla")
          ()))
      (14
       .
       #s(test
          1
          (check-true
           (string-prefix? (run-t "from N to Northeastern") "disambiguate"))
          ()))
      (15
       .
       #s(test
          1
          (check-true
           (string-prefix? (run-t "from Northeastern to N") "disambiguate"))
          ()))
      (16
       .
       #s(test
          1
          (check-true (string-prefix? (run-t "from N to G") "disambiguate"))
          ()))
      (17
       .
       #s(test
          1
          (check-true (string-prefix? (run-t "from N to N") "disambiguate"))
          ()))
      (18
       .
       #s(test
          1
          (check-equal?
           (run-t "disable blabla")
           "no such station to disable: blabla")
          ()))
      (19
       .
       #s(test
          1
          (check-true
           (string-prefix?
            (run-t "from Northeastern to Northeastern")
            "Close your eyes"))
          ()))
      (20
       .
       #s(test
          1
          (check-true
           (string-prefix?
            (run-t "from Northeaster to Northeastern")
            "Close your eyes"))
          ())))
