#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Research-Local/blgt-parent/gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
          ()))
      (1
       .
       #s(context 0 (begin (require rackunit (only-in racket/format ~a))) ()))
      (2 . #s(test 1 (check-true (if (exit? 'exit) #t #f)) ()))
      (3 . #s(test 1 (check-true (if (exit? 'quit) #t #f)) ()))
      (4 . #s(test 1 (check-true (if (exit? 'q) #t #f)) ()))
      (5 . #s(test 1 (check-false (exit? '())) ()))
      (6 . #s(test 1 (check-false (exit? #f)) ()))
      (7 . #s(test 1 (check-false (exit? 53)) ()))
      (8 . #s(test 1 (check-false (exit? 'hello)) ()))
      (9
       .
       #s(test
          1
          (check-true (eq? 'exit (get-field id (find-command CMD* 'exit))))
          ()))
      (10
       .
       #s(test
          1
          (check-true (eq? 'dup (get-field id (find-command CMD* 'dup))))
          ()))
      (11
       .
       #s(test
          1
          (check-true (eq? '+ (get-field id (find-command CMD* '+))))
          ()))
      (12 . #s(test 1 (check-false (if (find-command CMD* 'hi) #t #f)) ()))
      (13 . #s(test 1 (check-false (if (find-command CMD* "yes") #t #f)) ()))
      (14 . #s(test 1 (check-false (if (find-command CMD* 0) #t #f)) ()))
      (15 . #s(test 1 (check-true (if (help? 'help) #t #f)) ()))
      (16 . #s(test 1 (check-true (if (help? '?) #t #f)) ()))
      (17 . #s(test 1 (check-true (if (help? '--help) #t #f)) ()))
      (18 . #s(test 1 (check-false (help? 'exit)) ()))
      (19 . #s(test 1 (check-false (help? #f)) ()))
      (20 . #s(test 1 (check-false (help? 'q)) ()))
      (21 . #s(test 1 (check-false (help? 21)) ()))
      (22 . #s(test 1 (check-true (if (show? 'show) #t #f)) ()))
      (23 . #s(test 1 (check-true (if (show? 'ls) #t #f)) ()))
      (24 . #s(test 1 (check-true (if (show? 'print) #t #f)) ()))
      (25 . #s(test 1 (check-false (show? 'exit)) ()))
      (26 . #s(test 1 (check-false (show? #f)) ()))
      (27 . #s(test 1 (check-false (show? 'q)) ()))
      (28 . #s(test 1 (check-false (show? 12)) ()))
      (29
       .
       #s(test
          1
          (check-equal?
           (length (string-split (show-help CMD*) "\n"))
           (+ 1 (length CMD*)))
          ()))
      (30
       .
       #s(test
          1
          (check-equal?
           (length (string-split (show-help CMD* #f) "\n"))
           (+ 1 (length CMD*)))
          ()))
      (31
       .
       #s(test
          1
          (check-regexp-match #rx"^Cannot help" (show-help CMD* "booo"))
          ()))
      (32
       .
       #s(test
          1
          (check-regexp-match #rx"^Unknown command" (show-help CMD* 'booo))
          ()))
      (33
       .
       #s(test
          1
          (check-regexp-match #rx"^Print help" (show-help CMD* 'help))
          ())))
