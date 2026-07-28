#hash((0
       .
       #s(target-file "./benchmarks/morsecode/original/levenshtein.rkt" ()))
      (1 . #s(context 0 (begin (require rackunit)) ()))
      (2
       .
       #s(test
          1
          (check-equal? (vector-levenshtein '#(6 6 6) '#(6 35 6 24 6 32)) 3)
          ()))
      (3 . #s(context 1 (begin (require rackunit)) ()))
      (4
       .
       #s(test
          3
          (check-equal? (list-levenshtein/eq '(b c e x f y) '(a b c d e f)) 4)
          ()))
      (5 . #s(context 3 (begin (require rackunit)) ()))
      (6
       .
       #s(test 5 (check-equal? (string-levenshtein "adresse" "address") 2) ()))
      (7 . #s(context 5 (begin (require rackunit)) ()))
      (8
       .
       #s(test
          7
          (check-equal?
           (%string-levenshtein/predicate "ABCD" "aBXcD" char-ci=?)
           1)
          ()))
      (9
       .
       #s(test
          7
          (check-equal?
           (vector-levenshtein/predicate
            #(#\A #\B #\C #\D)
            #(#\a #\B #\X #\c #\D)
            char-ci=?)
           1)
          ()))
      (10
       .
       #s(test
          7
          (check-equal?
           (list-levenshtein/predicate
            '(#\A #\B #\C #\D)
            '(#\a #\B #\X #\c #\D)
            char-ci=?)
           1)
          ()))
      (11 . #s(context 7 (begin (require rackunit) (define g "gumbo")) ()))
      (12 . #s(test 11 (check-equal? (levenshtein g "gambol") 2) ()))
      (13 . #s(test 11 (check-equal? (levenshtein g "dumbo") 1) ()))
      (14 . #s(test 11 (check-equal? (levenshtein g "umbrage") 5) ())))
