#hash((0
       .
       #s(target-file
          "/home/breitnw/Documents/research/parent-cat/gtp-benchmarks/benchmarks/sieve/original/main.rkt"
          ()))
      (1
       .
       #s(context
          0
          (begin (require rackunit) (define counter (count-from 2)))
          ()))
      (2 . #s(test 1 (check-equal? 2 (simple-stream-get counter 0)) ()))
      (3 . #s(test 1 (check-equal? 6 (simple-stream-get counter 4)) ()))
      (4
       .
       #s(test
          1
          (check-equal? '(2 3 4 5 6) (simple-stream-take counter 5))
          ()))
      (5
       .
       #s(test
          1
          (check-equal? '(2 4) (simple-stream-take (sift 3 counter) 2))
          ()))
      (6
       .
       #s(test
          1
          (check-equal?
           '(2 4 5 7 8 10 11 13 14)
           (simple-stream-take (sift 3 counter) 9))
          ()))
      (7
       .
       #s(test
          1
          (check-equal? '(3 5 7 9 11) (simple-stream-take (sift 2 counter) 5))
          ()))
      (8
       .
       #s(test
          1
          (check-equal? '(2 3 4 6 7) (simple-stream-take (sift 5 counter) 5))
          ()))
      (9
       .
       #s(test
          1
          (check-equal?
           '(5 6 7 8 9 11 13)
           (simple-stream-take (sieve (count-from 5)) 7))
          ()))
      (10
       .
       #s(context
          1
          (begin
            (define multiples-of-two
              (let loop ((n 4))
                (make-simple-stream n (lambda () (loop (+ n 2)))))))
          ()))
      (11
       .
       #s(test
          10
          (check-equal?
           '(4 6 10 14 22 26 34)
           (simple-stream-take (sieve multiples-of-two) 7))
          ()))
      (12
       .
       #s(test
          10
          (check-equal?
           '(2 3 5 7 11 13 17 19 23 29)
           (simple-stream-take primes 10))
          ())))
