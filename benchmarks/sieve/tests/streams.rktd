#hash((0
       .
       #s(target-file
          "/home/breitnw/Documents/research/parent-cat/gtp-benchmarks/benchmarks/sieve/original/streams.rkt"
          ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define (ones) (make-simple-stream 1 ones))
            (define-values
             (first-ones rest-ones)
             (simple-stream-unfold (ones))))
          ()))
      (2 . #s(test 1 (check-equal? 1 first-ones) ()))
      (3
       .
       #s(context
          1
          (begin
            (define-values
             (first-ones-2 _rest-ones-2)
             (simple-stream-unfold rest-ones)))
          ()))
      (4 . #s(test 3 (check-equal? 1 first-ones-2) ()))
      (5 . #s(test 3 (check-equal? 1 (simple-stream-get (ones) 0)) ()))
      (6 . #s(test 3 (check-equal? 1 (simple-stream-get (ones) 4)) ()))
      (7 . #s(test 3 (check-equal? '() (simple-stream-take (ones) 0)) ()))
      (8
       .
       #s(test 3 (check-equal? '(1 1 1 1 1) (simple-stream-take (ones) 5)) ()))
      (9
       .
       #s(context
          3
          (begin
            (define powers-of-2
              (let next ((n 1)) (make-simple-stream n (λ () (next (* n 2))))))
            (define-values
             (first-pow rest-pow)
             (simple-stream-unfold powers-of-2)))
          ()))
      (10 . #s(test 9 (check-equal? 1 first-pow) ()))
      (11
       .
       #s(context
          9
          (begin
            (define-values
             (first-pow-2 rest-pow-2)
             (simple-stream-unfold rest-pow)))
          ()))
      (12 . #s(test 11 (check-equal? 2 first-pow-2) ()))
      (13
       .
       #s(context
          11
          (begin
            (define-values
             (first-pow-3 _rest-pow-3)
             (simple-stream-unfold rest-pow-2)))
          ()))
      (14 . #s(test 13 (check-equal? 4 first-pow-3) ()))
      (15 . #s(test 13 (check-equal? 1 (simple-stream-get powers-of-2 0)) ()))
      (16 . #s(test 13 (check-equal? 2 (simple-stream-get powers-of-2 1)) ()))
      (17
       .
       #s(test 13 (check-equal? 1024 (simple-stream-get powers-of-2 10)) ()))
      (18
       .
       #s(test 13 (check-equal? '() (simple-stream-take powers-of-2 0)) ()))
      (19
       .
       #s(test 13 (check-equal? '(1 2) (simple-stream-take powers-of-2 2)) ()))
      (20
       .
       #s(test
          13
          (check-equal? '(1 2 4 8 16) (simple-stream-take powers-of-2 5))
          ()))
      (21
       .
       #s(test
          13
          (check-equal? '(4 8 16 32 64) (simple-stream-take rest-pow-2 5))
          ())))
