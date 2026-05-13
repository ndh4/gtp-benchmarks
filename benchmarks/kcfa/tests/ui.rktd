#hash((0 . #s(target-file "./benchmarks/kcfa/original/ui.rkt" ()))
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
       #s(test 1 (check-equal? (format-mono-store empty-mono-store) "") ()))
      (3
       .
       #s(context
          1
          (begin
            (define init-state__smallest
              (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '()))
            (define states__smallest
              (explore (set) (list init-state__smallest))))
          ()))
      (4
       .
       #s(test
          3
          (check-equal?
           states__smallest
           (set (State (Lam 'g1 '(a) (Ref 'g0 'a)) '#hash() '#hash() '())))
          ()))
      (5
       .
       #s(context
          3
          (begin (define summary__smallest (summarize states__smallest)))
          ()))
      (6 . #s(test 5 (check-equal? summary__smallest '#hash()) ()))
      (7
       .
       #s(context
          5
          (begin
            (define mono-store__smallest
              (monovariant-store summary__smallest)))
          ()))
      (8 . #s(test 7 (check-equal? mono-store__smallest '#hash()) ()))
      (9
       .
       #s(context
          7
          (begin
            (define init-state__tiny
              (State
               (Call
                'g6
                (Lam 'g3 '(a) (Ref 'g2 'a))
                (list (Lam 'g5 '(b) (Ref 'g4 'b))))
               '#hash()
               '#hash()
               '()))
            (define states__tiny (explore (set) (list init-state__tiny))))
          ()))
      (10
       .
       #s(test
          9
          (check-true
           (sets-equal?
            states__tiny
            (set
             (State
              (Call
               'g6
               (Lam 'g3 '(a) (Ref 'g2 'a))
               (list (Lam 'g5 '(b) (Ref 'g4 'b))))
              '#hash()
              '#hash()
              '())
             (State
              (Ref 'g2 'a)
              (hash 'a (Binding 'a '(g6)))
              (hash
               (Binding 'a '(g6))
               (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash())))
              '(g6)))))
          ()))
      (11
       .
       #s(context
          9
          (begin (define summary__tiny (summarize states__tiny)))
          ()))
      (12
       .
       #s(test
          11
          (check-equal?
           summary__tiny
           (hash
            (Binding 'a '(g6))
            (set (Closure (Lam 'g5 '(b) (Ref 'g4 'b)) '#hash()))))
          ()))
      (13
       .
       #s(context
          11
          (begin (define mono-store__tiny (monovariant-store summary__tiny)))
          ()))
      (14
       .
       #s(test
          13
          (check-equal?
           mono-store__tiny
           (hash 'a (set (Lam 'g5 '(b) (Ref 'g4 'b)))))
          ()))
      (15
       .
       #s(context
          13
          (begin
            (define init-state__standard
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
               '()))
            (define states__standard
              (explore (set) (list init-state__standard))))
          ()))
      (16
       .
       #s(test
          15
          (check-true
           (sets-equal?
            states__standard
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
          ()))
      (17
       .
       #s(context
          15
          (begin (define summary__standard (summarize states__standard)))
          ()))
      (18
       .
       #s(test
          17
          (check-equal?
           summary__standard
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
              (hash 'a (Binding 'a '(g9)) 'id (Binding 'id '(g23)))))))
          ()))
      (19
       .
       #s(context
          17
          (begin
            (define mono-store__standard
              (monovariant-store summary__standard)))
          ()))
      (20
       .
       #s(test
          19
          (check-equal?
           mono-store__standard
           (hash
            'a
            (set (Lam 'g13 '(z) (Ref 'g12 'z)))
            'b
            (set (Lam 'g16 '(y) (Ref 'g15 'y)))
            'id
            (set (Lam 'g10 '(x k) (Call 'g9 (Ref 'g7 'k) (list (Ref 'g8 'x)))))
            'k
            (set
             (Lam
              'g20
              '(a)
              (Call
               'g19
               (Ref 'g14 'id)
               (list
                (Lam 'g16 '(y) (Ref 'g15 'y))
                (Lam 'g18 '(b) (Ref 'g17 'b)))))
             (Lam 'g18 '(b) (Ref 'g17 'b)))
            'x
            (set (Lam 'g13 '(z) (Ref 'g12 'z)) (Lam 'g16 '(y) (Ref 'g15 'y)))))
          ())))
