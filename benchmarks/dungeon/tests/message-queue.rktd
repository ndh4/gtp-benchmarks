#hash((0
       .
       #s(target-file "./benchmarks/dungeon/original/message-queue.rkt" ()))
      (1 . #s(context 0 (begin (require rackunit)) ()))
      (2 . #s(test 1 (check-equal? (unbox message-queue) '()) ()))
      (3
       .
       #s(context
          1
          (begin
            (enqueue-message! "A very important message")
            (enqueue-message! "Another very important message"))
          ()))
      (4
       .
       #s(test
          3
          (check-equal?
           (unbox message-queue)
           (list "Another very important message" "A very important message"))
          ()))
      (5 . #s(context 3 (begin (reset-message-queue!)) ()))
      (6 . #s(test 5 (check-equal? (unbox message-queue) '()) ())))
