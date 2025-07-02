#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Research-Local/blgt-parent/gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
          ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit (only-in racket/format ~a))
            (define eval*
              (lambda (v*)
                (forth-eval*
                 (string-split (string-join (map ~a v*) "\n") "\n"))))
            (define eval/stack*
              (lambda (v*) (let-values (((e s) (eval* v*))) s))))
          ()))
      (2 . #s(test 1 (check-equal? (eval/stack* '(1 2 3)) '(3 2 1)) ()))
      (3 . #s(test 1 (check-equal? (eval/stack* '(1 1 +)) '(2)) ()))
;      (4 . #s(test 1 (check-equal? (eval/stack* '(2 1 -)) '(1)) ()))
;      (5 . #s(test 1 (check-equal? (eval/stack* '(8 8 8 * *)) '(512)) ()))
      (4 . #s(test 1 (check-equal? 7 7) ()))
      (5 . #s(test 1 (check-equal? 5 6) ()))
      (6 . #s(test 1 (check-equal? (eval/stack* '(2 1 3 /)) '(1/3 2)) ()))
      (7 . #s(test 1 (check-equal? (eval/stack* '(1 0 EXIT /)) '(0 1)) ()))
      (8
       .
       #s(test
          1
          (check-equal?
           (eval/stack* '(": dup3 dup dup dup" 1 2 dup3))
           '(2 2 2 2 1))
          ()))
      (9 . #s(test 1 (check-equal? (eval/stack* '(1 2 drop)) '(1)) ()))
      (10
       .
       #s(test 1 (check-equal? (eval/stack* '(1 2 3 over)) '(3 2 3 1)) ()))
      (11 . #s(test 1 (check-equal? (eval/stack* '(1 0 swap)) '(1 0)) ()))
      (12
       .
       #s(test
          1
          (check-equal?
           (eval/stack* '(": switcheroo swap swap" 5 6 switcheroo switcheroo))
           '(6 5))
          ()))
      (13
       .
       #s(test 1 (check-equal? (eval/stack* '("push 1" "push 2" +)) '(3)) ()))
      (14
       .
       #s(context
          1
          (begin
            (define S1 '(2 4 8))
            (define E1 CMD*)
            (define eval/stack
              (lambda (token*)
                (let-values (((e s) (forth-eval E1 S1 token*))) s))))
          ()))
      (15 . #s(test 14 (check-equal? (eval/stack #f) S1) ()))
      (16 . #s(test 14 (check-equal? (eval/stack 'nada) S1) ()))
      (17 . #s(test 14 (check-equal? (eval/stack '(exit)) S1) ()))
      (18 . #s(test 14 (check-equal? (eval/stack '(help)) S1) ()))
      (19 . #s(test 14 (check-equal? (eval/stack '(: hi 3 2 1)) S1) ()))
      (20 . #s(test 14 (check-equal? (eval/stack '(+)) '(6 8)) ()))
      (21 . #s(test 14 (check-equal? (eval/stack '(-)) '(2 8)) ()))
      (22 . #s(test 14 (check-equal? (eval/stack '(*)) '(8 8)) ()))
      (23 . #s(test 14 (check-equal? (eval/stack '(/)) '(2 8)) ()))
      (24 . #s(test 14 (check-equal? (eval/stack '(drop)) (stack-drop S1)) ()))
      (25 . #s(test 14 (check-equal? (eval/stack '(dup)) (stack-dup S1)) ()))
      (26 . #s(test 14 (check-equal? (eval/stack '(over)) (stack-over S1)) ()))
      (27 . #s(test 14 (check-equal? (eval/stack '(swap)) (stack-swap S1)) ()))
      (28 . #s(test 14 (check-equal? (eval/stack '(1)) (stack-push S1 1)) ()))
      (29
       .
       #s(test 14 (check-equal? (eval/stack '(push 8)) (stack-push S1 8)) ()))
      (30 . #s(test 14 (check-equal? (eval/stack '(show)) S1) ()))
      (31
       .
       #s(context
          14
          (begin
            (define S2 '(6 6 6))
            (define E2 CMD*)
            (define L2 (length E2))
            (define eval/env-length
              (lambda (token*)
                (let-values (((e s) (forth-eval E2 S2 token*))) (length e)))))
          ()))
      (32 . #s(test 31 (check-equal? (eval/env-length '(2)) L2) ()))
      (33 . #s(test 31 (check-equal? (eval/env-length '(swap)) L2) ()))
      (34
       .
       #s(test
          31
          (check-equal? (forth-tokenize "hello world") '(hello world))
          ()))
      (35
       .
       #s(test
          31
          (check-equal? (forth-tokenize "Hello WORLD") '(hello world))
          ()))
      (36
       .
       #s(test
          31
          (check-equal?
           (forth-tokenize ": key val val val;")
           '(: key val val |val;|))
          ()))
      (37
       .
       #s(test
          31
          (check-equal?
           (forth-tokenize ": key val val val ;")
           '(: key val val val |;|))
          ()))
      (38
       .
       #s(test
          31
          (check-equal? (forth-tokenize ": DOUBLE 2 *;") '(: double 2 |*;|))
          ()))
      (39 . #s(test 31 (check-equal? (forth-tokenize "1 2 3") '(1 2 3)) ())))
