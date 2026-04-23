#hash((0 . #s(target-file "./benchmarks/mbta/original/t-graph.rkt" ()))
      (1 . #s(context 0 (begin (require rackunit)) ()))
      (2
       .
       #s(test
          1
          (check-equal? (line-specification? "---- blue") '("blue"))
          ()))
      (3 . #s(test 1 (check-equal? (line-specification? "----blue") #f) ()))
      (4
       .
       #s(test 1 (check-equal? (line-specification? "- blue") '("blue")) ()))
      (5
       .
       #s(test
          1
          (check-equal? (line-specification? "- A B C") '("A" "B" "C"))
          ()))
      (6
       .
       #s(test
          1
          (check-exn exn:fail? (λ () (lines->hash '("---- line "))))
          ()))
      (7
       .
       #s(test
          1
          (check-equal?
           '#hash(("line" . ("Davis Station")))
           (lines->hash '("-- line " "Davis Station")))
          ()))
      (8
       .
       #s(test
          1
          (check-equal?
           '#hash(("line"
                   .
                   ("Davis Station"
                    ("Alewife Station" "Davis Station")
                    ("Davis Station" "Alewife Station"))))
           (lines->hash '("---- line " "Alewife Station" "Davis Station")))
          ()))
      (9
       .
       #s(test
          1
          (check-equal?
           '#hash(("line1"
                   .
                   ("Davis Station"
                    ("Alewife Station" "Davis Station")
                    ("Davis Station" "Alewife Station")))
                  ("line2"
                   .
                   ("Davis Station"
                    ("Alewife Station" "Davis Station")
                    ("Davis Station" "Alewife Station"))))
           (lines->hash
            '("---- line1 line2 " "Alewife Station" "Davis Station")))
          ()))
      (10
       .
       #s(test
          1
          (check-exn
           exn:fail?
           (λ ()
             (lines->hash
              '("-- line1 line2 "
                "Alewife Station"
                "---- line3 "
                "Government Center Station"))))
          ()))
      (11
       .
       #s(test
          1
          (check-equal?
           '#hash(("line1"
                   .
                   ("Government Center Station"
                    ("Alewife Station" "Government Center Station")
                    ("Government Center Station" "Alewife Station")))
                  ("line2"
                   .
                   ("Davis Station"
                    ("Alewife Station" "Davis Station")
                    ("Davis Station" "Alewife Station"))))
           (lines->hash
            '("-- line1 line2 "
              "Alewife Station"
              "---- line1 "
              "Government Center Station"
              "---- line2 "
              "Davis Station")))
          ()))
      (12
       .
       #s(test
          1
          (check-equal?
           '(("blue"
              (("Government Center Station" "Bowdoin Station")
               ("Bowdoin Station" "Government Center Station")
               ("State Station" "Government Center Station")
               ("Government Center Station" "State Station")
               ("Aquarium Station" "State Station")
               ("State Station" "Aquarium Station")
               ("Maverick Station" "Aquarium Station")
               ("Aquarium Station" "Maverick Station")
               ("Airport Station" "Maverick Station")
               ("Maverick Station" "Airport Station")
               ("Wood Island Station" "Airport Station")
               ("Airport Station" "Wood Island Station")
               ("Orient Heights Station" "Wood Island Station")
               ("Wood Island Station" "Orient Heights Station")
               ("Suffolk Downs Station" "Orient Heights Station")
               ("Orient Heights Station" "Suffolk Downs Station")
               ("Beachmont Station" "Suffolk Downs Station")
               ("Suffolk Downs Station" "Beachmont Station")
               ("Revere Beach Station" "Beachmont Station")
               ("Beachmont Station" "Revere Beach Station")
               ("Wonderland Station" "Revere Beach Station")
               ("Revere Beach Station" "Wonderland Station"))))
           (read-t-line-from-file "blue"))
          ()))
      (13 . #s(context 1 (begin (define graph (read-t-graph))) ()))
      (14 . #s(test 13 (check-equal? (send graph station "Oops") '()) ()))
      (15
       .
       #s(test
          13
          (check-equal?
           (send graph station "Northeastern University Station")
           "Northeastern University Station")
          ()))
      (16
       .
       #s(test
          13
          (check-equal?
           (send graph station "Northeastern")
           "Northeastern University Station")
          ()))
      (17
       .
       #s(test
          13
          (check-equal?
           (send graph station "Center")
           '("Hynes Convention Center"
             "Government Center Station"
             "Quincy Center Station"
             "Malden Center Station"
             "Tufts Medical Center Station"))
          ()))
      (18
       .
       #s(test 13 (check-equal? (send graph station? "Northeastern") #f) ()))
      (19
       .
       #s(test
          13
          (check-equal?
           (send graph station? "Northeastern University Station")
           #t)
          ()))
      (20
       .
       #s(test
          13
          (check-equal?
           `(("Government Center Station" ,(set)))
           (send graph find-path
             "Government Center Station"
             "Government Center Station"))
          ()))
      (21
       .
       #s(test
          13
          (check-equal?
           `((("Northeastern University Station" ,(set "E"))
              ("Symphony Station" ,(set "E"))))
           (send graph find-path
             "Northeastern University Station"
             "Symphony Station"))
          ()))
      (22
       .
       #s(test
          13
          (check-equal?
           `((("Northeastern University Station" ,(set "E"))
              ("Symphony Station" ,(set "E"))
              ("Prudential Station" ,(set "E"))))
           (send graph find-path
             "Northeastern University Station"
             "Prudential Station"))
          ()))
      (23
       .
       #s(context
          13
          (begin
            (define multiple-routes
              (send graph find-path
                "Government Center Station"
                "Haymarket Station")))
          ()))
      (24
       .
       #s(test
          23
          (check-not-false
           (member
            `(("Government Center Station" ,(set "D" "E" "B" "C"))
              ("Park Street Station" ,(set "D" "E" "B" "C"))
              ("Downtown Crossing Station" ,(set "Mattapan" "Braintree"))
              ("State Station" ,(set "orange"))
              ("Haymarket Station" ,(set "orange")))
            multiple-routes))
          ()))
      (25
       .
       #s(test
          23
          (check-not-false
           (member
            `(("Government Center Station" ,(set "D" "E" "B" "C"))
              ("Haymarket Station" ,(set "D" "E" "B" "C")))
            multiple-routes))
          ()))
      (26
       .
       #s(test
          23
          (check-not-false
           (member
            `(("Government Center Station" ,(set "blue"))
              ("State Station" ,(set "blue"))
              ("Haymarket Station" ,(set "orange")))
            multiple-routes))
          ())))
