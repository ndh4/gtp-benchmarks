#lang racket


;; (provide
;;   command%
;;   CMD*
;; )

;; -----------------------------------------------------------------------------

(require
 racket/match
 racket/class
 (only-in racket/string string-join string-split)
 (for-syntax racket/base racket/syntax syntax/parse)
 ;; racket/contract
 "../../../ctcs/configurable.rkt"
 "../../../ctcs/precision-config.rkt"
 (only-in racket/function curry)
 (only-in racket/list empty? first second rest)
 (only-in "../../../ctcs/common.rkt"
          class/c*
          or-#f/c
          command%/c
          command%?
          command%?-with-exec
          stack?
          env?
          list-with-min-size/c
          equal?/c)
)
;; (require (only-in "stack.rkt"
;;   stack-drop
;;   stack-dup
;;   stack-init
;;   stack-over
;;   stack-pop
;;   stack-push
;;   stack-swap
;; ))
(require/configurable-contract "stack.rkt" stack-swap stack-push stack-pop stack-over stack-init stack-dup stack-drop )

(provide/configurable-contract
 [assert ([max any/c]
   [types any/c])]
 [command% ([max command%/c]
   [types command%/c])]
 [singleton-list? ([max (->i ([x any/c])
             [result (x)
                     (and (list? x) (not (empty? x)) (empty? (rest x)))])]
   [types (any/c . -> . boolean?)])]
 [binop-command% ([max binop-command%/c]
   [types binop-command%/c])]
 [CMD* ([max (and/c env?
               (list/c
                ;; exit
                (command%?-with-exec
                 (args E S v)
                 [result (if (or (eof-object? v)
                                 (is-or-starts-with? exit? v))
                             'EXIT
                             #f)])
                ;; help
                (command%?-with-exec
                 (args E S v)
                 [result (if (is-or-starts-with? help? v)
                             (equal?/c (cons E S))
                             #f)])

                (binop-command%/c-for +)
                (binop-command%/c-for -)
                (binop-command%/c-for *)

                ;; drop
                (command%?-with-exec
                 (args E S v)
                 [result (if (and (is-or-starts-with? (curry equal? 'drop) v)
                                  ((list-with-min-size/c 1) S))
                             (equal?/c (cons E (rest S)))
                             #f)])
                ;; dup
                (command%?-with-exec
                 (args E S v)
                 [result (if (and (is-or-starts-with? (curry equal? 'dup) v)
                                  ((list-with-min-size/c 1) S))
                             (equal?/c (cons E (cons (first S) S)))
                             #f)])
                ;; over
                (command%?-with-exec
                 (args E S v)
                 [result (if (and (is-or-starts-with? (curry equal? 'over) v)
                                  ((list-with-min-size/c 2) S))
                             (equal?/c
                              (cons E (cons (first S)
                                            (cons (second S)
                                                  (cons (first S)
                                                        (rest (rest S)))))))
                             #f)])
                ;; swap
                (command%?-with-exec
                 (args E S v)
                 [result (if (and (is-or-starts-with? (curry equal? 'swap) v)
                                  ((list-with-min-size/c 2) S))
                             (equal?/c
                              (cons E
                                    (cons (second S)
                                          (cons (first S)
                                                (rest (rest S))))))
                             #f)])
                ;; push
                (command%?-with-exec
                 (args E S v)
                 [result (if (or (and (list? v)
                                      (>= (length v) 1)
                                      (exact-integer? (first v)))
                                 (and (list? v)
                                      (>= (length v) 2)
                                      (equal? (first v) 'push)
                                      (exact-integer? (second v))))
                             (equal?/c
                              (cons E
                                    (cons (if (exact-integer? (first v))
                                              (first v)
                                              (second v))
                                          S)))
                             #f)])
                ;; show
                ;; lltemporal: prints
                (command%?-with-exec
                 (args E S v)
                 [result (if (is-or-starts-with? (curry equal? 'show) v)
                             (equal?/c (cons E S))
                             #f)])))]
   [types env?])]
 [exit? ([max (->i ([sym any/c])
             [result (sym)
                     (memq sym '(exit quit q leave bye))])]
   [types (any/c . -> . boolean?)])]
 [find-command ([max (->i ([E env?]
              [sym symbol?])
             [result (E)
                     (and (not (empty? E))
                          (get-field id (first E)))])]
   [types (env? symbol? . -> . symbol?)])]
 [help? ([max (->i ([sym any/c])
             [result (sym)
                     (memq sym '(help ? ??? -help --help h))])]
   [types (any/c . -> . boolean?)])]
 [show? ([max (->i ([sym any/c])
             [result (sym)
                     (memq sym '(show print pp ls stack))])]
   [types (any/c . -> . boolean?)])]
 [show-help ([max (->i ([E env?]
              [v any/c])
             [result string?]
             #:post (E v result)
             (regexp-match?
              (match v
                [#f (and (= (length (string-split result "\n"))
                            (add1 (length E)))
                         "^Available commands:")]
                [(or (list (? symbol? s)) (? symbol? s))
                 (regexp-quote
                  (if (find-command E s)
                      (get-field descr (find-command E s))
                      (format "Unknown command '~a'" s)))]
                [x
                 (regexp-quote
                  (format "Cannot help with '~a'" x))])
              result))]
   [types (env? any/c . -> . string?)])])

(define (assert v p)
  (unless (p v) (error 'assert))
  v)

;; =============================================================================
;; -- Commands

(define command%
  (class object%
    (super-new)
    (init-field
      id
      descr
      exec)))

(define/ctc-helper ((env-with/c cmd-ids) env)
  (cond [(env? env)
         (define env-cmd-ids
           (for/list ([env-cmd (in-list env)])
             (get-field id env-cmd)))
         (for/and ([c (in-list cmd-ids)])
           (member c env-cmd-ids))]
        [else #f]))



;; True if the argument is a list with one element
(define (singleton-list? x)
  (and (list? x)
       (not (null? x))
       (null? (cdr x))))

;; Create a binary operation command.
;; Command is recognized by its identifier,
;;  the identifier is then applied to the top 2 numbers on the stack.
(define/ctc-helper binop-command%/c
  (and/c command%/c
         ;; original: depends on an extension of class/c
         #;(class/dc
          (init-field [binop (number? number? . -> . number?)])
          (inherit-field [id symbol?]
                         [binop (number? number? . -> . number?)])
          (field [id symbol?]
                 [exec (binop id)
                       (->i ([E env?] [S stack?] [v any/c])
                            [result (E S v)
                                    (match* {S v}
                                      [{(list-rest v1 v2 S-rest)
                                        (list (== id eq?))}
                                       (equal?/c
                                        (cons E (cons (binop v2 v1) S-rest)))]
                                      [{_ _} #f])])]))
         (class/c
          (init-field [binop (number? number? . -> . number?)])
          (inherit-field [id symbol?]
                         [binop (number? number? . -> . number?)])
          (field [id symbol?]
                 [exec (->i ([E env?] [S stack?] [v any/c])
                            [result (E S v)
                                    (match* {S v}
                                      [{(list-rest v1 v2 S-rest)
                                        (list symbol?)}
                                       (or/c #f ;; if the symbol above is not == id
                                             (cons/c (equal?/c E)
                                                     (cons/c number?
                                                             (equal?/c S-rest))))]
                                      [{_ _} #f])])]))))

(require (for-syntax syntax/parse))
(define-syntax/ctc-helper (binop-command%/c-for stx)
  (syntax-parse stx
    [(_ binop)
     #'(command%?-with-exec
        (type binop-command%/c)
        (args E S v)
        [result
         (or-#f/c
          ;; lltodo: example of bad error reporting: add extra parens
          (if ((list-with-min-size/c 2) S)
              (equal?/c
               (cons E
                     (cons (binop (second S) (first S))
                           (rest (rest S)))))
              #f))])]))

(define binop-command%
  (class command%
    (init-field
     binop)
    (super-new
      (id (assert (object-name binop) symbol?))
      (exec (lambda (E S v)
              (if (singleton-list? v)
                  (if (eq? (car v) (get-field id this))
                      (let*-values ([(v1 S1) (stack-pop S)]
                                    [(v2 S2) (stack-pop S1)])
                        (cons E (stack-push S2 (binop v2 v1))))
                      #f)
                  #f))))))

;; Turns a symbol into a stack command parser
(define-syntax make-stack-command
  (syntax-parser
   [(_ opcode:id d:str)
    #:with stack-cmd (format-id #'opcode "stack-~a" (syntax-e #'opcode))
    #`(new command%
        (id '#,(syntax-e #'opcode))
        (descr d)
        (exec (lambda (E S v)
          (and (singleton-list? v)
               (eq? '#,(syntax-e #'opcode) (car v))
               (cons E (stack-cmd S))))))]))

(define/ctc-helper (is-or-starts-with? predicate v)
  (or (and (symbol? v)
           (predicate v))
      (and (list? v)
           (not (empty? v))
           (predicate (first v)))))

;; Default environment of commands
(define CMD*
  (list
   (new command%
        (id 'exit)
        (descr "End the REPL session")
        (exec (lambda (E S v)
                (if (or (eof-object? v)
                        (and (symbol? v)
                             (exit? v))
                        (and (list? v)
                             (not (null? v))
                             (exit? (car v))))
                    'EXIT
                    #f))))
   (new command%
        (id 'help)
        (descr "Print help information")
        (exec (lambda (E S v)
                (cond
                  [(and (symbol? v) (help? v))
                   (displayln (show-help E))
                   (cons E S)]
                  [(and (list? v) (not (null? v)) (help? (car v)))
                   (displayln (show-help E (and (not (null? (cdr v))) (cdr v))))
                   (cons E S)]
                  [else
                   #f]))))
   (instantiate binop-command% (+)
     (descr "Add the top two numbers on the stack"))
   (instantiate binop-command% (-)
     (descr "Subtract the top item of the stack from the second item."))
   (instantiate binop-command% (*)
     (descr "Multiply the top two item on the stack."))
   #;(instantiate binop-command% (/)
       (descr "Divide the top item of the stack by the second item."))
   (make-stack-command drop
                       "Drop the top item from the stack")
   (make-stack-command dup
                       "Duplicate the top item of the stack")
   (make-stack-command over
                       "Duplicate the top item of the stack, but place the duplicate in the third position of the stack.")
   (make-stack-command swap
                       "Swap the first two numbers on the stack")
   (new command%
        (id 'push)
        (descr "Push a number onto the stack")
        (exec (lambda (E S v)
                (match v
                  [`(push ,(? exact-integer? n))
                   (cons E (stack-push S n))]
                  [`(,(? exact-integer? n))
                   (cons E (stack-push S n))]
                  [_ #f]))))
   (new command%
        (id 'show)
        (descr "Print the current stack")
        (exec (lambda (E S v)
                (match v
                  [`(,(? show?))
                   (displayln S)
                   (cons E S)]
                  [_ #f]))))
   ))

(define (exit? sym)
  (and (memq sym '(exit quit q leave bye)) #t))

;; Search the environment for a command with `id` equal to `sym`
(define (find-command E sym)
  (for/or ([c (in-list E)])
    (get-field id c) (error 'no)))
    ;(if (eq? sym (get-field id c)) c #f)))

(define (help? sym)
  (and (memq sym '(help ? ??? -help --help h)) #t))

(define (show? sym)
  (and (memq sym '(show print pp ls stack)) #t))

;; Print a help message.
;; If the optional argument is given, try to print information about it.
(define (show-help E [v #f])
  (match v
    [#f
     (string-join
      (for/list ([c (in-list E)])
        (format "    ~a : ~a" (get-field id c) (get-field descr c)))
      "\n"
      #:before-first "Available commands:\n")]
    [(or (list (? symbol? s)) (? symbol? s))
     (define c (find-command E (assert s symbol?)))
     (if c
         (get-field descr c)
         (format "Unknown command '~a'" s))]
    [x
     (format "Cannot help with '~a'" x)]))

;; TESTS

(module+ test
  (require
   rackunit
   rackunit-abbrevs
   (only-in racket/format ~a))

  ;; -- exit?
  (check-true* (lambda (x) (if (exit? x) #t #f))
   ['exit]
   ['quit]
   ['q])

  (check-false* exit?
   ['()]
   [#f]
   [53]
   ['hello])

  ;; -- find-command
  (check-true* (lambda (sym) (eq? sym (command-id (find-command CMD* sym))))
   ['exit]
   ['dup]
   ['+])

  (check-false* (lambda (sym) (if (find-command CMD* sym) #t #f))
   ['hi]
   ['hi]
   ["yes"]
   [00])

  ;; -- help?
  (check-true* (lambda (v) (if (help? v) #t #f))
   ['help]
   ['?]
   ['--help])

  (check-false* help?
   ['exit]
   [#f]
   ['q]
   [21])

  ;; -- show?
  (check-true* (lambda (v) (if (show? v) #t #f))
   ['show]
   ['ls]
   ['print])

  (check-false* help?
   ['exit]
   [#f]
   ['q]
   [12])

  ;; -- show-help
  (check-equal? (length (string-split (show-help CMD*) "\n")) (+ 1 (length CMD*)))
  (check-equal? (length (string-split (show-help CMD* #f) "\n")) (+ 1 (length CMD*)))
  (check-regexp-match #rx"^Cannot help" (show-help CMD* "booo"))
  (check-regexp-match #rx"^Unknown command" (show-help CMD* 'booo))
  (check-regexp-match #rx"^Print help" (show-help CMD* 'help))

  ;; -- forth-eval*
  (let* ([eval* (lambda (v*)
                  (with-input-from-string (string-join (map ~a v*) "\n")
                    (lambda () (forth-eval* (current-input-port)))))]
         [eval/stack (lambda (v*) (let-values ([(e s) (eval* v*)]) s))])
    (check-apply* eval/stack
     ['(1 2 3)
      == '(3 2 1)]
     ['(1 1 +)
      == '(2)]
     ['(2 1 -)
      == '(1)]
     ['(8 8 8 * *)
      == '(512)]
     ['(2 1 3 /)
      == '(1/3 2)]
     ['(1 0 EXIT /)
      == '(0 1)]
     ['(": dup3 dup dup dup" 1 2 dup3)
      == '(2 2 2 2 1)]
     ['(1 2 drop)
      == '(1)]
     ['(1 2 3 over)
      == '(3 2 3 1)]
     ['(1 0 swap)
      == '(1 0)]
     ['(": switcheroo swap swap" 5 6 switcheroo switcheroo)
      == '(6 5)]
     ['("push 1" "push 2" +)
      == '(3)]))

  ;; -- forth-eval
  (let* ([S '(2 4 8)]
         [E CMD*]
         [eval/stack (lambda (token*)
                       (let-values ([(e s) (forth-eval E S token*)]) s))])
  (check-apply* eval/stack
   [#f
    == S]
   ['nada
    == S]
   ['(exit)
    == S]
   ['(help)
    == S]
   ['(: hi 3 2 1)
    == S]
   ['(+)
    == '(6 8)]
   ['(-)
    == '(2 8)]
   ['(*)
    == '(8 8)]
   ['(/)
    == '(2 8)]
   ['(drop)
    == (stack-drop S)]
   ['(dup)
    == (stack-dup S)]
   ['(over)
    == (stack-over S)]
   ['(swap)
    == (stack-swap S)]
   ['(1)
    == (stack-push S 1)]
   ['(push 8)
    == (stack-push S 8)]
   ['(show)
    == S]))

  (let* ([S '(6 6 6)]
         [E CMD*]
         [L (length E)]
         [eval/env-length (lambda (token*)
                     (let-values ([(e s) (forth-eval E S token*)]) (length e)))])
    (check-apply* eval/env-length
     ['(2)
      == L]
     ['(: new dup drop swap)
      == (+ 1 L)]
     ['(swap)
      == L]))

  ;; -- forth-tokenize
  (check-apply* forth-tokenize
   ["hello world" == '(hello world)]
   ["Hello WORLD" == '(hello world)]
   [": key val val val;" == '(: key val val |val;|)]
   [": key val val val ;" == '(: key val val val |;|)]
   [": DOUBLE 2 *;" == '(: double 2 |*;|)]
   ["1 2 3" == '(1 2 3)])

)