#hash((0
       .
       #s(target-file
          "/home/breitnw/Documents/research/parent-cat/gtp-benchmarks/benchmarks/mbta/original/t-view.rkt"
          ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define manage1 (new manage%))
            (define manage1_disable (get-field disabled manage1)))
          ()))
      (2 . #s(test 1 (check-equal? '() manage1_disable) ()))
      (3
       .
       #s(context
          1
          (begin
            (define dis_add_out1 (send manage1 add-to-disabled "Government")))
          ()))
      (4 . #s(test 3 (check-equal? dis_add_out1 #f) ()))
      (5
       .
       #s(test
          3
          (check-equal?
           '("Government Center Station")
           (get-field disabled manage1))
          ()))
      (6
       .
       #s(context
          3
          (begin
            (define dis_rem_out1
              (send manage1 remove-from-disabled "Haymarket")))
          ()))
      (7 . #s(test 6 (check-equal? dis_rem_out1 #f) ()))
      (8
       .
       #s(test
          6
          (check-equal?
           '("Government Center Station")
           (get-field disabled manage1))
          ()))
      (9
       .
       #s(context
          6
          (begin
            (define dis_rem_out2
              (send manage1 remove-from-disabled "Government Center")))
          ()))
      (10 . #s(test 9 (check-equal? dis_rem_out2 #f) ()))
      (11 . #s(test 9 (check-equal? '() (get-field disabled manage1)) ()))
      (12
       .
       #s(context
          9
          (begin
            (define dis_rem_out3
              (send manage1 remove-from-disabled "Government Center")))
          ()))
      (13 . #s(test 12 (check-equal? dis_rem_out3 #f) ()))
      (14 . #s(test 12 (check-equal? '() (get-field disabled manage1)) ()))
      (15
       .
       #s(test 12 (check-equal? (length (get-field disabled manage1)) 0) ()))
      (16
       .
       #s(context
          12
          (begin
            (define dis_rem_out4 (send manage1 add-to-disabled "Government")))
          ()))
      (17
       .
       #s(test 16 (check-equal? (length (get-field disabled manage1)) 1) ()))
      (18
       .
       #s(context
          16
          (begin
            (define dis_rem_out4_2 (send manage1 add-to-disabled "Riverway")))
          ()))
      (19 . #s(test 18 (check-equal? dis_rem_out4 dis_rem_out4_2) ()))
      (20 . #s(test 18 (check-equal? dis_rem_out4 #f) ()))
      (21 . #s(test 18 (check-equal? #t (boolean? dis_rem_out4)) ()))
      (22
       .
       #s(test
          18
          (check-equal?
           '("Riverway Station" "Government Center Station")
           (get-field disabled manage1))
          ()))
      (23
       .
       #s(test 18 (check-equal? (length (get-field disabled manage1)) 2) ()))
      (24
       .
       #s(test
          18
          (check-equal?
           "it is currently impossible to reach Government Center Station from Riverway Station via subways"
           (send manage1 find "Riverway" "Government"))
          ()))
      (25
       .
       #s(test
          18
          (check-equal?
           "it is currently impossible to reach Bowdoin Station from Government Center Station via subways"
           (send manage1 find "Government" "Bowdoin"))
          ()))
      (26
       .
       #s(test
          18
          (check-equal?
           "it is currently impossible to reach Government Center Station from Bowdoin Station via subways"
           (send manage1 find "Bowdoin" "Government"))
          ()))
      (27
       .
       #s(test
          18
          (check-equal?
           "it is currently impossible to reach Bowdoin Station from Riverway Station via subways"
           (send manage1 find "Riverway" "Bowdoin"))
          ()))
      (28
       .
       #s(test
          18
          (check-equal?
           "it is currently impossible to reach Riverway Station from Bowdoin Station via subways"
           (send manage1 find "Bowdoin" "Riverway"))
          ()))
      (29
       .
       #s(test
          18
          (check-equal?
           "it is currently impossible to reach Suffolk Downs Station from Bowdoin Station via subways"
           (send manage1 find "Bowdoin" "Suffolk"))
          ()))
      (30
       .
       #s(context
          18
          (begin
            (define attempt (send manage1 remove-from-disabled "Government")))
          ()))
      (31
       .
       #s(test 30 (check-equal? (length (get-field disabled manage1)) 1) ()))
      (32
       .
       #s(test
          30
          (check-equal? '("Riverway Station") (get-field disabled manage1))
          ()))
      (33
       .
       #s(test
          30
          (check-equal?
           "Close your eyes and tap your heels three times. Open your eyes. You will be at Government."
           (send manage1 find "Government" "Government"))
          ()))
      (34
       .
       #s(test
          30
          (check-equal?
           "it is currently impossible to reach Government Center Station from Riverway Station via subways"
           (send manage1 find "Riverway" "Government"))
          ()))
      (35
       .
       #s(test
          30
          (check-equal?
           "it is currently impossible to reach Riverway Station from Government Center Station via subways"
           (send manage1 find "Government" "Riverway"))
          ()))
      (36
       .
       #s(test
          30
          (check-equal?
           "it is currently impossible to reach Bowdoin Station from Riverway Station via subways"
           (send manage1 find "Riverway" "Bowdoin"))
          ()))
      (37
       .
       #s(test
          30
          (check-equal?
           "it is currently impossible to reach Riverway Station from Bowdoin Station via subways"
           (send manage1 find "Bowdoin" "Riverway"))
          ()))
      (38
       .
       #s(test
          30
          (check-equal?
           "Government Center Station, take blue\nBowdoin Station, take blue"
           (send manage1 find "Government" "Bowdoin"))
          ()))
      (39
       .
       #s(test
          30
          (check-equal?
           "Bowdoin Station, take blue\nGovernment Center Station, take blue"
           (send manage1 find "Bowdoin" "Government"))
          ()))
      (40
       .
       #s(test
          30
          (check-equal?
           (send manage1 add-to-disabled "brooke")
           "no such station to disable: brooke")
          ()))
      (41
       .
       #s(test 30 (check-equal? (length (get-field disabled manage1)) 1) ()))
      (42
       .
       #s(test
          30
          (check-equal?
           (send manage1 remove-from-disabled "brooke")
           "no such station to enable: brooke")
          ()))
      (43
       .
       #s(test 30 (check-equal? (length (get-field disabled manage1)) 1) ()))
      (44
       .
       #s(test
          30
          (check-equal?
           "no such destination: brooke"
           (send manage1 find "Government" "brooke"))
          ()))
      (45
       .
       #s(test
          30
          (check-equal?
           "no such destination: brooke"
           (send manage1 find "Riverway" "brooke"))
          ()))
      (46
       .
       #s(test
          30
          (check-equal?
           "clarify station to be disabled: Brookline Hills Station Stony Brook Station Brookline Village Station"
           (send manage1 add-to-disabled "Brook"))
          ()))
      (47
       .
       #s(test
          30
          (check-equal? #t (string? (send manage1 add-to-disabled "Brook")))
          ()))
      (48
       .
       #s(test 30 (check-equal? (length (get-field disabled manage1)) 1) ()))
      (49
       .
       #s(test
          30
          (check-equal? '("Riverway Station") (get-field disabled manage1))
          ()))
      (50
       .
       #s(context
          30
          (begin
            (define dis_rem_out5_1
              (send manage1 add-to-disabled "Brookline Hills")))
          ()))
      (51
       .
       #s(test 50 (check-equal? (length (get-field disabled manage1)) 2) ()))
      (52
       .
       #s(test
          50
          (check-equal?
           '("Brookline Hills Station" "Riverway Station")
           (get-field disabled manage1))
          ()))
      (53
       .
       #s(context
          50
          (begin
            (define dis_rem_out5_2
              (send manage1 add-to-disabled "Stony Brook")))
          ()))
      (54
       .
       #s(test 53 (check-equal? (length (get-field disabled manage1)) 3) ()))
      (55
       .
       #s(test
          53
          (check-equal?
           '("Stony Brook Station"
             "Brookline Hills Station"
             "Riverway Station")
           (get-field disabled manage1))
          ()))
      (56
       .
       #s(context
          53
          (begin
            (define dis_rem_out5_3
              (send manage1 add-to-disabled "Brookline Village")))
          ()))
      (57
       .
       #s(test 56 (check-equal? (length (get-field disabled manage1)) 4) ()))
      (58
       .
       #s(test
          56
          (check-equal?
           '("Brookline Village Station"
             "Stony Brook Station"
             "Brookline Hills Station"
             "Riverway Station")
           (get-field disabled manage1))
          ()))
      (59
       .
       #s(test
          56
          (check-equal?
           "Close your eyes and tap your heels three times. Open your eyes. You will be at brook."
           (send manage1 find "brook" "brook"))
          ()))
      (60
       .
       #s(test
          56
          (check-equal?
           "Close your eyes and tap your heels three times. Open your eyes. You will be at Brook."
           (send manage1 find "Brook" "Brook"))
          ()))
      (61
       .
       #s(test
          56
          (check-equal?
           "Close your eyes and tap your heels three times. Open your eyes. You will be at Riverway."
           (send manage1 find "Riverway" "Riverway"))
          ()))
      (62
       .
       #s(test
          56
          (check-equal?
           "no such destination: brookline hills"
           (send manage1 find "Stony Brook" "brookline hills"))
          ()))
      (63
       .
       #s(test
          56
          (check-equal?
           "it is currently impossible to reach Brookline Hills Station from Stony Brook Station via subways"
           (send manage1 find "Stony Brook" "Brookline Hills"))
          ()))
      (64
       .
       #s(context
          56
          (begin (define ts1 (send manage1 add-to-disabled "Riverway")))
          ()))
      (65 . #s(test 64 (check-equal? #f ts1) ()))
      (66
       .
       #s(test 64 (check-equal? (length (get-field disabled manage1)) 5) ()))
      (67
       .
       #s(test
          64
          (check-equal?
           '("Riverway Station"
             "Brookline Village Station"
             "Stony Brook Station"
             "Brookline Hills Station"
             "Riverway Station")
           (get-field disabled manage1))
          ()))
      (68
       .
       #s(context
          64
          (begin (define ts2 (send manage1 remove-from-disabled "Riverway")))
          ()))
      (69 . #s(test 68 (check-equal? #f ts2) ()))
      (70
       .
       #s(test
          68
          (check-equal?
           #t
           (and (boolean? (send manage1 remove-from-disabled "Riverway"))
                (equal? #f (send manage1 remove-from-disabled "Riverway"))))
          ()))
      (71
       .
       #s(test 68 (check-equal? (length (get-field disabled manage1)) 3) ()))
      (72
       .
       #s(test
          68
          (check-equal?
           '("Brookline Village Station"
             "Stony Brook Station"
             "Brookline Hills Station")
           (get-field disabled manage1))
          ()))
      (73
       .
       #s(context
          68
          (begin (define ts3 (send manage1 remove-from-disabled "Riverway")))
          ()))
      (74 . #s(test 73 (check-equal? #f ts3) ()))
      (75
       .
       #s(test 73 (check-equal? (length (get-field disabled manage1)) 3) ()))
      (76
       .
       #s(test
          73
          (check-equal?
           '("Brookline Village Station"
             "Stony Brook Station"
             "Brookline Hills Station")
           (get-field disabled manage1))
          ()))
      (77
       .
       #s(test
          73
          (check-equal?
           "no such station to enable: brook"
           (send manage1 remove-from-disabled "brook"))
          ()))
      (78
       .
       #s(test
          73
          (check-equal?
           #t
           (string? (send manage1 remove-from-disabled "brook")))
          ()))
      (79
       .
       #s(test 73 (check-equal? (length (get-field disabled manage1)) 3) ()))
      (80
       .
       #s(test
          73
          (check-equal?
           '("Brookline Village Station"
             "Stony Brook Station"
             "Brookline Hills Station")
           (get-field disabled manage1))
          ()))
      (81
       .
       #s(test
          73
          (check-equal?
           '("Brookline Village Station"
             "Stony Brook Station"
             "Brookline Hills Station")
           (get-field disabled manage1))
          ()))
      (82
       .
       #s(test
          73
          (check-equal?
           "disambiguate your destination: Brookline Hills Station Stony Brook Station Brookline Village Station"
           (send manage1 find "Riverway" "Brook"))
          ()))
      (83
       .
       #s(test
          73
          (check-equal?
           "disambiguate your current location: Cleveland Circle Station Brigham Circle Station"
           (send manage1 find "Circle" "Brook"))
          ()))
      (84
       .
       #s(test
          73
          (check-equal?
           "disambiguate your current location: Cleveland Circle Station Brigham Circle Station"
           (send manage1 find "Circle" "brook"))
          ()))
      (85
       .
       #s(test
          73
          (check-equal?
           "disambiguate your destination: Brookline Hills Station Stony Brook Station Brookline Village Station"
           (send manage1 find "riverway" "Brook"))
          ()))
      (86
       .
       #s(test 73 (check-equal? (length (get-field disabled manage1)) 3) ()))
      (87
       .
       #s(test
          73
          (check-equal?
           "no such destination: brook"
           (send manage1 find "Riverway" "brook"))
          ()))
      (88
       .
       #s(test
          73
          (check-equal?
           "no such station: riverway"
           (send manage1 find "riverway" "brook"))
          ()))
      (89
       .
       #s(test
          73
          (check-equal?
           "no such station to disable: brook"
           (send manage1 add-to-disabled "brook"))
          ()))
      (90
       .
       #s(test 73 (check-equal? (length (get-field disabled manage1)) 3) ()))
      (91
       .
       #s(context
          73
          (begin (define ts4 (send manage1 add-to-disabled "Sym")))
          ()))
      (92 . #s(test 91 (check-equal? #f ts4) ()))
      (93
       .
       #s(test 91 (check-equal? (length (get-field disabled manage1)) 4) ()))
      (94
       .
       #s(test
          91
          (check-equal?
           '("Symphony Station"
             "Brookline Village Station"
             "Stony Brook Station"
             "Brookline Hills Station")
           (get-field disabled manage1))
          ()))
      (95
       .
       #s(context
          91
          (begin (define ts5 (send manage1 remove-from-disabled "phony")))
          ()))
      (96 . #s(test 95 (check-equal? #f ts5) ()))
      (97
       .
       #s(test 95 (check-equal? (length (get-field disabled manage1)) 3) ()))
      (98
       .
       #s(test
          95
          (check-equal?
           '("Brookline Village Station"
             "Stony Brook Station"
             "Brookline Hills Station")
           (get-field disabled manage1))
          ())))
