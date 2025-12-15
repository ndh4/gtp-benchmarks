#hash((0
       .
       #s(target-file
          "/Users/nhejduk/Research-Local/blgt-parent/gtp-benchmarks/benchmarks/forth/original/stack.rkt"
          ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define exn-rx #rx"empty stack")
            (define-syntax-rule
             (check-stack-exn e)
             (check-exn exn-rx (lambda () e)))
            (define S (list->stack '(1 2))))
          ()))
      (2 . #s(test 1 (check-equal? (stack-drop S) (list->stack '(2))) ()))
      (3
       .
       #s(test
          1
          (check-equal? (stack-drop (stack-drop S)) (list->stack '()))
          ()))
      (4
       .
       #s(test
          1
          (check-stack-exn (stack-drop (stack-drop (stack-drop S))))
          ()))
      (5 . #s(test 1 (check-equal? (stack-dup S) (list->stack '(1 1 2))) ()))
      (6
       .
       #s(test
          1
          (check-equal? (stack-dup (stack-drop S)) (list->stack '(2 2)))
          ()))
      (7 . #s(test 1 (check-stack-exn (stack-dup (stack-init))) ()))
      (8 . #s(test 1 (check-equal? (stack-init) (list->stack '())) ()))
      (9 . #s(test 1 (check-equal? (stack-over S) (list->stack '(1 2 1))) ()))
      (10 . #s(test 1 (check-stack-exn (stack-over (stack-drop S))) ()))
      (11 . #s(test 1 (check-stack-exn (stack-over (stack-init))) ()))
      (12
       .
       #s(context
          1
          (begin
            (let-values (((v S2) (stack-pop S)))
              (check-equal? v 1)
              (check-equal? S2 (list->stack '(2)))))
          ()))
      (13
       .
       #s(test
          12
          (check-stack-exn (stack-pop (stack-drop (stack-drop S))))
          ()))
      (14 . #s(test 12 (check-stack-exn (stack-pop (stack-init))) ()))
      (15
       .
       #s(test 12 (check-equal? (stack-push S 4) (list->stack '(4 1 2))) ()))
      (16
       .
       #s(test
          12
          (check-equal? (stack-push (stack-init) 6) (list->stack '(6)))
          ()))
      (17 . #s(test 12 (check-equal? (stack-swap S) (list->stack '(2 1))) ()))
      (18 . #s(test 12 (check-equal? (stack-swap (stack-swap S)) S) ()))
      (19 . #s(test 12 (check-stack-exn (stack-swap (stack-drop S))) ()))
      (20 . #s(test 12 (check-stack-exn (stack-swap (stack-init))) ())))
