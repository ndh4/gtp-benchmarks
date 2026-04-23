#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Documents/Research-Cloud/teco-parent/gtp-benchmarks/benchmarks/morsecode/original/morse-code-strings.rkt"
          ()))
      (1 . #s(context 0 (begin (require rackunit)) ()))
      (2 . #s(test 1 (check-equal? (char->dit-dah-string #\a) ".-") ()))
      (3 . #s(test 1 (check-equal? (char->dit-dah-string #\b) "-...") ()))
      (4 . #s(test 1 (check-equal? (char->dit-dah-string #\s) "...") ()))
      (5 . #s(test 1 (check-equal? (char->dit-dah-string #\S) "...") ()))
      (6 . #s(test 1 (check-equal? (string->morse "sos") "...---...") ()))
      (7
       .
       #s(test
          1
          (check-equal?
           (string->morse "wrought?")
           ".--.-.---..---.....-..--..")
          ()))
      (8
       .
       #s(test
          1
          (check-equal?
           (string->morse "WROUGHT?")
           ".--.-.---..---.....-..--..")
          ())))
