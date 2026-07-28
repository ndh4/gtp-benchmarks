#hash((0 . #s(target-file "./benchmarks/forth/original/stack.rkt" ()))
      (1
       .
       #s(context
          0
          (begin
            (require rackunit)
            (define exn-rx #rx"empty stack")
            (define S (list->stack '(1 2))))
          ()))
      (2 . #s(test 1 (check-equal? (stack-drop S) (list->stack '(2))) ()))
      (3
       .
       #s(test
          1
          (check-equal? (stack-drop (stack-drop S)) (list->stack '()))
          ()))
      (4 . #s(test 1 (check-equal? (stack-dup S) (list->stack '(1 1 2))) ()))
      (5
       .
       #s(test
          1
          (check-equal? (stack-dup (stack-drop S)) (list->stack '(2 2)))
          ()))
      (6 . #s(test 1 (check-equal? (stack-init) (list->stack '())) ()))
      (7 . #s(test 1 (check-equal? (stack-over S) (list->stack '(1 2 1))) ()))
      (8
       .
       #s(context
          1
          (begin
            (let-values (((v S2) (stack-pop S)))
              (check-equal? v 1)
              (check-equal? S2 (list->stack '(2)))))
          ()))
      (9
       .
       #s(test 8 (check-equal? (stack-push S 4) (list->stack '(4 1 2))) ()))
      (10
       .
       #s(test
          8
          (check-equal? (stack-push (stack-init) 6) (list->stack '(6)))
          ()))
      (11 . #s(test 8 (check-equal? (stack-swap S) (list->stack '(2 1))) ()))
      (12 . #s(test 8 (check-equal? (stack-swap (stack-swap S)) S) ())))
