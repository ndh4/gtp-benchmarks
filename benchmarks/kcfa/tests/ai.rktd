#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Documents/Research-Cloud/teco-parent/contracts-and-testing/bex/orchestration/../../../gtp-benchmarks/benchmarks/kcfa/original/ai.rkt"
          ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit (only-in racket/format ~a))
            (define (sets-equal? s1 s2)
              (and (all-in? (set->list s1) (set->list s2))
                   (all-in? (set->list s2) (set->list s1))))
            (define (all-in? l1 l2)
              (not (not (for/and ((elem l1)) (member elem l2))))))
          ()))
      (2
       .
       #s(test
          1
          (check-equal?
           ((atom-eval
             (hash 'id (Binding 'id '(g23)))
             (hash
              (Binding 'id '(g23))
              (set
               (Closure
                (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                '#hash()))))
            (Lam 'g13 '(z) (Ref 'g12 'z)))
           (set
            (Closure
             (Lam 'g13 '(z) (Ref 'g12 'z))
             (hash 'id (Binding 'id '(g23))))))
          ()))
      (3
       .
       #s(test
          1
          (check-exn
           #rx"atom-eval got a plain Exp"
           (λ ()
             ((atom-eval
               (hash 'id (Binding 'id '(g23)))
               (hash
                (Binding 'id '(g23))
                (set
                 (Closure
                  (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                  '#hash()))))
              (Call
               'g21
               (Ref 'g11 'id)
               (list
                (Lam 'g13 '(z) (Ref 'g12 'z))
                (Lam
                 'g20
                 '(a)
                 (Call
                  'g19
                  (Ref 'g14 'id)
                  (list
                   (Lam 'g16 '(y) (Ref 'g15 'y))
                   (Lam 'g18 '(b) (Ref 'g17 'b))))))))))
          ()))
      (4
       .
       #s(test
          1
          (check-equal?
           ((atom-eval
             (hash 'k (Binding 'k '(g19)) 'x (Binding 'x '(g19)))
             (hash
              (Binding 'a '(g9))
              (set
               (Closure
                (Lam 'g13 '(z) (Ref 'g12 'z))
                (hash 'id (Binding 'id '(g23)))))
              (Binding 'id '(g23))
              (set
               (Closure
                (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                '#hash()))
              (Binding 'k '(g21))
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
                (hash 'id (Binding 'id '(g23)))))
              (Binding 'x '(g19))
              (set
               (Closure
                (Lam 'g16 '(y) (Ref 'g15 'y))
                (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
              (Binding 'x '(g21))
              (set
               (Closure
                (Lam 'g13 '(z) (Ref 'g12 'z))
                (hash 'id (Binding 'id '(g23)))))
              (Binding 'k '(g19))
              (set
               (Closure
                (Lam 'g18 '(b) (Ref 'g17 'b))
                (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))))
            (Ref 'g7 'k))
           (set
            (Closure
             (Lam 'g18 '(b) (Ref 'g17 'b))
             (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23))))))
          ()))
      (5
       .
       #s(test
          1
          (check-equal?
           (next (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '()))
           (set))
          ()))
      (6
       .
       #s(test
          1
          (check-true
           (sets-equal?
            (next
             (State
              (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x)))
              (hash 'k (Binding 'k '(g19)) 'x (Binding 'x '(g19)))
              (hash
               (Binding 'a '(g9))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'id '(g23))
               (set
                (Closure
                 (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                 '#hash()))
               (Binding 'k '(g21))
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
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'x '(g19))
               (set
                (Closure
                 (Lam 'g16 '(y) (Ref 'g15 'y))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
               (Binding 'x '(g21))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'k '(g19))
               (set
                (Closure
                 (Lam 'g18 '(b) (Ref 'g17 'b))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23))))))
              '(g19)))
            (set
             (State
              (Ref 'g17 'b)
              (hash
               'a
               (Binding 'a '(g9))
               'b
               (Binding 'b '(g9))
               'id
               (Binding 'id '(g23)))
              (hash
               (Binding 'a '(g9))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'b '(g9))
               (set
                (Closure
                 (Lam 'g16 '(y) (Ref 'g15 'y))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
               (Binding 'id '(g23))
               (set
                (Closure
                 (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                 '#hash()))
               (Binding 'k '(g21))
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
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'x '(g19))
               (set
                (Closure
                 (Lam 'g16 '(y) (Ref 'g15 'y))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
               (Binding 'x '(g21))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'k '(g19))
               (set
                (Closure
                 (Lam 'g18 '(b) (Ref 'g17 'b))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23))))))
              '(g9)))))
          ()))
      (7
       .
       #s(test
          1
          (check-equal?
           (explore
            (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '()))
            '())
           (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
          ()))
      (8
       .
       #s(test
          1
          (check-equal?
           (explore
            (set)
            (list (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
           (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
          ()))
      (9
       .
       #s(test
          1
          (check-equal?
           (explore
            (set)
            (list
             (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())
             (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
           (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
          ()))
      (10
       .
       #s(test
          1
          (check-true
           (sets-equal?
            (explore
             (set)
             (list
              (State
               (Call
                'g23
                (Lam
                 'g22
                 '(id)
                 (Call
                  'g21
                  (Ref 'g11 'id)
                  (list
                   (Lam 'g13 '(z) (Ref 'g12 'z))
                   (Lam
                    'g20
                    '(a)
                    (Call
                     'g19
                     (Ref 'g14 'id)
                     (list
                      (Lam 'g16 '(y) (Ref 'g15 'y))
                      (Lam 'g18 '(b) (Ref 'g17 'b))))))))
                (list
                 (Lam
                  'g10
                  '(x k)
                  (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))))
               '#hash()
               '#hash()
               '())))
            (set
             (State
              (Call
               'g21
               (Ref 'g11 'id)
               (list
                (Lam 'g13 '(z) (Ref 'g12 'z))
                (Lam
                 'g20
                 '(a)
                 (Call
                  'g19
                  (Ref 'g14 'id)
                  (list
                   (Lam 'g16 '(y) (Ref 'g15 'y))
                   (Lam 'g18 '(b) (Ref 'g17 'b)))))))
              (hash 'id (Binding 'id '(g23)))
              (hash
               (Binding 'id '(g23))
               (set
                (Closure
                 (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                 '#hash())))
              '(g23))
             (State
              (Ref 'g17 'b)
              (hash
               'a
               (Binding 'a '(g9))
               'b
               (Binding 'b '(g9))
               'id
               (Binding 'id '(g23)))
              (hash
               (Binding 'a '(g9))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'b '(g9))
               (set
                (Closure
                 (Lam 'g16 '(y) (Ref 'g15 'y))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
               (Binding 'id '(g23))
               (set
                (Closure
                 (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                 '#hash()))
               (Binding 'k '(g21))
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
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'x '(g19))
               (set
                (Closure
                 (Lam 'g16 '(y) (Ref 'g15 'y))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
               (Binding 'x '(g21))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'k '(g19))
               (set
                (Closure
                 (Lam 'g18 '(b) (Ref 'g17 'b))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23))))))
              '(g9))
             (State
              (Call
               'g19
               (Ref 'g14 'id)
               (list
                (Lam 'g16 '(y) (Ref 'g15 'y))
                (Lam 'g18 '(b) (Ref 'g17 'b))))
              (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))
              (hash
               (Binding 'a '(g9))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'id '(g23))
               (set
                (Closure
                 (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                 '#hash()))
               (Binding 'k '(g21))
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
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'x '(g21))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23))))))
              '(g9))
             (State
              (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x)))
              (hash 'k (Binding 'k '(g19)) 'x (Binding 'x '(g19)))
              (hash
               (Binding 'a '(g9))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'id '(g23))
               (set
                (Closure
                 (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                 '#hash()))
               (Binding 'k '(g21))
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
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'x '(g19))
               (set
                (Closure
                 (Lam 'g16 '(y) (Ref 'g15 'y))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))
               (Binding 'x '(g21))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'k '(g19))
               (set
                (Closure
                 (Lam 'g18 '(b) (Ref 'g17 'b))
                 (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23))))))
              '(g19))
             (State
              (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x)))
              (hash 'k (Binding 'k '(g21)) 'x (Binding 'x '(g21)))
              (hash
               (Binding 'id '(g23))
               (set
                (Closure
                 (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))
                 '#hash()))
               (Binding 'k '(g21))
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
                 (hash 'id (Binding 'id '(g23)))))
               (Binding 'x '(g21))
               (set
                (Closure
                 (Lam 'g13 '(z) (Ref 'g12 'z))
                 (hash 'id (Binding 'id '(g23))))))
              '(g21))
             (State
              (Call
               'g23
               (Lam
                'g22
                '(id)
                (Call
                 'g21
                 (Ref 'g11 'id)
                 (list
                  (Lam 'g13 '(z) (Ref 'g12 'z))
                  (Lam
                   'g20
                   '(a)
                   (Call
                    'g19
                    (Ref 'g14 'id)
                    (list
                     (Lam 'g16 '(y) (Ref 'g15 'y))
                     (Lam 'g18 '(b) (Ref 'g17 'b))))))))
               (list
                (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x))))))
              '#hash()
              '#hash()
              '()))))
          ())))
