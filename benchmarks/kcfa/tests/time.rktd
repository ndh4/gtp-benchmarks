#hash((0 . #s(target-file "./benchmarks/kcfa/original/time.rkt" ()))
      (1
       .
       #s(context 0 (begin (require rackunit (only-in racket/format ~a))) ()))
      (2 . #s(test 1 (check-equal? (take* '(a b c d e f) 2) '(a b)) ()))
      (3 . #s(test 1 (check-equal? (take* '(a b c d e) 3) '(a b c)) ()))
      (4 . #s(test 1 (check-equal? time-zero '()) ()))
      (5 . #s(test 1 (check-equal? (tick (Stx 'g123) time-zero) '(g123)) ()))
      (6 . #s(test 1 (check-equal? (tick (Stx 'g123) '(g123)) '(g123)) ()))
      (7
       .
       #s(test 1 (check-equal? ((alloc '(g123)) 'x) (Binding 'x '(g123))) ())))
