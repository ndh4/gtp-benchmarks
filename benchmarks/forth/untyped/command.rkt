#lang racket

(require racket/match
         racket/class
         (only-in racket/string string-join string-split)
         (for-syntax racket/base racket/syntax syntax/parse)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         (only-in racket/function curry)
         (only-in racket/list empty? first second rest)
         (only-in
          "../../../ctcs/common.rkt"
          class/c*
          or-#f/c
          command%/c
          command%?
          command%-with-id/c
          command%?-with-exec
          stack?
          env?
          list-with-min-size/c
          equal?/c
          thunked-equal?/c))

(require (only-in
          "stack.rkt"
          stack-swap
          stack-push
          stack-pop
          stack-over
          stack-init
          stack-dup
          stack-drop))

(provide assert
         command%
         singleton-list?
         binop-command%
         CMD*
         exit?
         find-command
         help?
         show?
         show-help)

(define/contract
 (assert v p)
 (configurable-ctc (max any/c) (types any/c))
 (unless (p v) (error 'assert))
 v)

(define/contract
 command%
 (configurable-ctc (max command%/c) (types command%/c))
 (class object% (super-new) (init-field id descr exec)))

(define/ctc-helper
 ((env-with/c cmd-ids) env)
 (cond
  ((env? env)
   (define env-cmd-ids
     (for/list ((env-cmd (in-list env))) (get-field id env-cmd)))
   (for/and ((c (in-list cmd-ids))) (member c env-cmd-ids)))
  (else #f)))

(define/contract
 (singleton-list? x)
 (configurable-ctc
  (max
   (->i
    ((x any/c))
    (result (x) (and (list? x) (not (empty? x)) (empty? (rest x))))))
  (types (-> any/c boolean?)))
 (and (list? x) (not (null? x)) (null? (cdr x))))

(define/ctc-helper
 binop-command%/c
 (and/c
  command%/c
  (class/c
   (init-field (binop (-> number? number? number?)))
   (inherit-field (id symbol?) (binop (-> number? number? number?)))
   (field (id symbol?)
          (exec
           (->i
            ((E env?) (S stack?) (v any/c))
            (result
             (E S v)
             (match*
              (S v)
              (((list-rest v1 v2 S-rest) (list symbol?))
               (or/c
                #f
                (cons/c (equal?/c E) (cons/c number? (equal?/c S-rest)))))
              ((_ _) #f)))))))))

(require (for-syntax syntax/parse))

(define-syntax/ctc-helper
 (binop-command%/c-for stx)
 (syntax-parse
  stx
  ((_ binop)
   #'(command%?-with-exec
      (type binop-command%/c)
      (args E S v)
      (result
       (or-#f/c
        (if ((list-with-min-size/c 2) S)
          (thunked-equal?/c
           (thunk
            (cons E (cons (binop (second S) (first S)) (rest (rest S))))))
          #f)))))))

(define/contract
 binop-command%
 (configurable-ctc (max binop-command%/c) (types binop-command%/c))
 (class command%
   (init-field binop)
   (super-new
    (id (assert (object-name binop) symbol?))
    (exec
     (lambda (E S v)
       (if (singleton-list? v)
         (if (eq? (car v) (get-field id this))
           (let*-values (((v1 S1) (stack-pop S)) ((v2 S2) (stack-pop S1)))
             (cons E (stack-push S2 (binop v2 v1))))
           #f)
         #f))))))

(define-syntax make-stack-command
  (syntax-parser
   ((_ opcode:id d:str)
    #:with
    stack-cmd
    (format-id #'opcode "stack-~a" (syntax-e #'opcode))
    #`(new
       command%
       (id '#,(syntax-e #'opcode))
       (descr d)
       (exec
        (lambda (E S v)
          (and (singleton-list? v)
               (eq? '#,(syntax-e #'opcode) (car v))
               (cons E (stack-cmd S)))))))))

(define/ctc-helper
 (is-or-starts-with? predicate v)
 (or (and (symbol? v) (predicate v))
     (and (list? v) (not (empty? v)) (predicate (first v)))))

(define/contract
 CMD*
 (configurable-ctc
  (max
   (and/c
    env?
    (list/c
     (command%?-with-exec
      (args E S v)
      (result (if (or (eof-object? v) (is-or-starts-with? exit? v)) 'EXIT #f)))
     (command%?-with-exec
      (args E S v)
      (result (if (is-or-starts-with? help? v) (equal?/c (cons E S)) #f)))
     (binop-command%/c-for +)
     (binop-command%/c-for -)
     (binop-command%/c-for *)
     (binop-command%/c-for /)
     (command%?-with-exec
      (args E S v)
      (result
       (if (and (is-or-starts-with? (curry equal? 'drop) v)
                ((list-with-min-size/c 1) S))
         (equal?/c (cons E (rest S)))
         #f)))
     (command%?-with-exec
      (args E S v)
      (result
       (if (and (is-or-starts-with? (curry equal? 'dup) v)
                ((list-with-min-size/c 1) S))
         (equal?/c (cons E (cons (first S) S)))
         #f)))
     (command%?-with-exec
      (args E S v)
      (result
       (if (and (is-or-starts-with? (curry equal? 'over) v)
                ((list-with-min-size/c 2) S))
         (equal?/c
          (cons
           E
           (cons
            (first S)
            (cons (second S) (cons (first S) (rest (rest S)))))))
         #f)))
     (command%?-with-exec
      (args E S v)
      (result
       (if (and (is-or-starts-with? (curry equal? 'swap) v)
                ((list-with-min-size/c 2) S))
         (equal?/c (cons E (cons (second S) (cons (first S) (rest (rest S))))))
         #f)))
     (command%?-with-exec
      (args E S v)
      (result
       (if (or (and (list? v) (>= (length v) 1) (exact-integer? (first v)))
               (and (list? v)
                    (>= (length v) 2)
                    (equal? (first v) 'push)
                    (exact-integer? (second v))))
         (equal?/c
          (cons
           E
           (cons (if (exact-integer? (first v)) (first v) (second v)) S)))
         #f)))
     (command%?-with-exec
      (args E S v)
      (result
       (if (is-or-starts-with? (curry equal? 'show) v)
         (equal?/c (cons E S))
         #f))))))
  (types env?))
 (list
  (new
   command%
   (id 'exit)
   (descr "End the REPL session")
   (exec
    (lambda (E S v)
      (if (or (eof-object? v)
              (and (symbol? v) (exit? v))
              (and (list? v) (not (null? v)) (exit? (car v))))
        'EXIT
        #f))))
  (new
   command%
   (id 'help)
   (descr "Print help information")
   (exec
    (lambda (E S v)
      (cond
       ((and (symbol? v) (help? v)) (displayln (show-help E)) (cons E S))
       ((and (list? v) (not (null? v)) (help? (car v)))
        (displayln (show-help E (and (not (null? (cdr v))) (cdr v))))
        (cons E S))
       (else #f)))))
  (instantiate binop-command% (+)
    (descr "Add the top two numbers on the stack"))
  (instantiate binop-command% (-)
    (descr "Subtract the top item of the stack from the second item."))
  (instantiate binop-command% (*)
    (descr "Multiply the top two item on the stack."))
  (instantiate binop-command% (/)
    (descr "Divide the top item of the stack by the second item."))
  (make-stack-command drop "Drop the top item from the stack")
  (make-stack-command dup "Duplicate the top item of the stack")
  (make-stack-command
   over
   "Duplicate the top item of the stack, but place the duplicate in the third position of the stack.")
  (make-stack-command swap "Swap the first two numbers on the stack")
  (new
   command%
   (id 'push)
   (descr "Push a number onto the stack")
   (exec
    (lambda (E S v)
      (match
       v
       (`(push ,(? exact-integer? n)) (cons E (stack-push S n)))
       (`(,(? exact-integer? n)) (cons E (stack-push S n)))
       (_ #f)))))
  (new
   command%
   (id 'show)
   (descr "Print the current stack")
   (exec
    (lambda (E S v)
      (match v (`(,(? show?)) (displayln S) (cons E S)) (_ #f)))))))

(define/contract
 (exit? sym)
 (configurable-ctc
  (max
   (->i
    ((sym any/c))
    (result (sym) (not (not (memq sym '(exit quit q leave bye)))))))
  (types (-> any/c boolean?)))
 (and (memq sym '(exit quit q leave bye)) #t))

(define/contract
 (find-command E sym)
 (configurable-ctc
  (max
   (->i
    ((E env?) (sym symbol?))
    (result (E sym) (or-#f/c (command%-with-id/c sym)))))
  (types (-> env? symbol? command%)))
 (for/or ((c (in-list E))) (if (eq? sym (get-field id c)) c #f)))

(define/contract
 (help? sym)
 (configurable-ctc
  (max
   (->i
    ((sym any/c))
    (result (sym) (not (not (memq sym '(help ? ??? -help --help h)))))))
  (types (-> any/c boolean?)))
 (and (memq sym '(help ? ??? -help --help h)) #t))

(define/contract
 (show? sym)
 (configurable-ctc
  (max
   (->i
    ((sym any/c))
    (result (sym) (not (not (memq sym '(show print pp ls stack)))))))
  (types (-> any/c boolean?)))
 (and (memq sym '(show print pp ls stack)) #t))

(define/contract
 (show-help E (v #f))
 (configurable-ctc
  (max
   (->i
    ((E env?))
    ((v any/c))
    (result string?)
    #:post
    (E v result)
    (regexp-match?
     (match
      v
      ((or #f (? unsupplied-arg?))
       (and (= (length (string-split result "\n")) (add1 (length E)))
            "^Available commands:"))
      ((or (list (? symbol? s)) (? symbol? s))
       (regexp-quote
        (if (find-command E s)
          (get-field descr (find-command E s))
          (format "Unknown command '~a'" s))))
      (x (regexp-quote (format "Cannot help with '~a'" x))))
     result)))
  (types (-> env? any/c string?)))
 (match
  v
  (#f
   (string-join
    (for/list
     ((c (in-list E)))
     (format "    ~a : ~a" (get-field id c) (get-field descr c)))
    "\n"
    #:before-first
    "Available commands:\n"))
  ((or (list (? symbol? s)) (? symbol? s))
   (define c (find-command E (assert s symbol?)))
   (if c (get-field descr c) (format "Unknown command '~a'" s)))
  (x (format "Cannot help with '~a'" x))))

#;(module+
 test
 (require rackunit (only-in racket/format ~a))
 (check-true (if (exit? 'exit) #t #f))
 (check-true (if (exit? 'quit) #t #f))
 (check-true (if (exit? 'q) #t #f))
 (check-false (exit? '()))
 (check-false (exit? #f))
 (check-false (exit? 53))
 (check-false (exit? 'hello))
 (check-true (eq? 'exit (get-field id (find-command CMD* 'exit))))
 (check-true (eq? 'dup (get-field id (find-command CMD* 'dup))))
 (check-true (eq? '+ (get-field id (find-command CMD* '+))))
 (check-false (if (find-command CMD* 'hi) #t #f))
 (check-true (if (help? 'help) #t #f))
 (check-true (if (help? '?) #t #f))
 (check-true (if (help? '--help) #t #f))
 (check-false (help? 'exit))
 (check-false (help? #f))
 (check-false (help? 'q))
 (check-false (help? 21))
 (check-true (if (show? 'show) #t #f))
 (check-true (if (show? 'ls) #t #f))
 (check-true (if (show? 'print) #t #f))
 (check-false (show? 'exit))
 (check-false (show? #f))
 (check-false (show? 'q))
 (check-false (show? 12))
 (check-equal?
  (length (string-split (show-help CMD*) "\n"))
  (+ 1 (length CMD*)))
 (check-equal?
  (length (string-split (show-help CMD* #f) "\n"))
  (+ 1 (length CMD*)))
 (check-regexp-match #rx"^Cannot help" (show-help CMD* "booo"))
 (check-regexp-match #rx"^Unknown command" (show-help CMD* 'booo))
 (check-regexp-match #rx"^Print help" (show-help CMD* 'help)))

