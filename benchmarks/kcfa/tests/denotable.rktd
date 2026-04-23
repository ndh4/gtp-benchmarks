#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Documents/Research-Cloud/teco-parent/contracts-and-testing/bex/orchestration/../../../gtp-benchmarks/benchmarks/kcfa/original/denotable.rkt"
          ()))
      (1
       .
       #s(context 0 (begin (require rackunit (only-in racket/format ~a))) ()))
      (2
       .
       #s(test
          1
          (check-equal? (store-join empty-store empty-store) empty-store)
          ()))
      (3
       .
       #s(context
          1
          (begin
            (define s1
              (store-update
               empty-store
               (Binding 'a '(g6))
               (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())))))
          ()))
      (4
       .
       #s(test
          3
          (check-equal?
           (store-lookup s1 (Binding 'a '(g6)))
           (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())))
          ()))
      (5
       .
       #s(context
          3
          (begin
            (define s2
              (store-update*
               empty-store
               (list (Binding 'x '(g21)) (Binding 'k '(g21)))
               (list
                (set
                 (Closure
                  (Lam 'g13 '(z) (Ref 'g12 'z))
                  (hash 'id (Binding 'id '(g23)))))
                (set
                 (Closure
                  (Lam
                   'g20
                   '(a)
                   (Call
                    'g19
                    (Ref 'g14 'id)
                    (list
                     (Lam 'g16 '(y) (Ref 'g15 'y))
                     (Lam 'g18 '(b) (Ref 'g17 'b)))))
                  (hash 'id (Binding 'id '(g23)))))))))
          ()))
      (6
       .
       #s(test
          5
          (check-equal?
           (store-lookup s2 (Binding 'x '(g21)))
           (set
            (Closure
             (Lam 'g13 '(z) (Ref 'g12 'z))
             (hash 'id (Binding 'id '(g23))))))
          ()))
      (7
       .
       #s(test
          5
          (check-equal?
           (store-lookup s2 (Binding 'k '(g21)))
           (set
            (Closure
             (Lam
              'g20
              '(a)
              (Call
               'g19
               (Ref 'g14 'id)
               (list
                (Lam 'g16 '(y) (Ref 'g15 'y))
                (Lam 'g18 '(b) (Ref 'g17 'b)))))
             (hash 'id (Binding 'id '(g23))))))
          ()))
      (8 . #s(context 5 (begin (define s3 (store-join s1 s2))) ()))
      (9
       .
       #s(test
          8
          (check-equal?
           (store-lookup s3 (Binding 'a '(g6)))
           (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())))
          ()))
      (10
       .
       #s(test
          8
          (check-equal?
           (store-lookup s3 (Binding 'x '(g21)))
           (set
            (Closure
             (Lam 'g13 '(z) (Ref 'g12 'z))
             (hash 'id (Binding 'id '(g23))))))
          ()))
      (11
       .
       #s(test
          8
          (check-equal?
           (store-lookup s3 (Binding 'k '(g21)))
           (set
            (Closure
             (Lam
              'g20
              '(a)
              (Call
               'g19
               (Ref 'g14 'id)
               (list
                (Lam 'g16 '(y) (Ref 'g15 'y))
                (Lam 'g18 '(b) (Ref 'g17 'b)))))
             (hash 'id (Binding 'id '(g23))))))
          ()))
      (12
       .
       #s(context
          8
          (begin
            (define s4
              (store-update
               s3
               (Binding 'a '(g6))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23))))))))
          ()))
      (13
       .
       #s(test
          12
          (check-equal?
           (store-lookup s4 (Binding 'a '(g6)))
           (set
            (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())
            (Closure
             (Lam 'g13 '(z) (Ref 'g12 'z))
             (hash 'id (Binding 'id '(g23))))))
          ())))
