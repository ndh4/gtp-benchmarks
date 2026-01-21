#lang racket

(require #;racket/contract
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt")



;; Simple streams library.
;; For building and using infinite lists.

(provide/configurable-contract
 [make-simple-stream {[max (->i ([hd any/c]
                                 [thunk (-> simple-stream?)])
                                [result (hd thunk)
                                        (simple-stream/c (equal?/c hd) (equal?/c thunk))])]
                      [types (-> any/c (-> simple-stream?) simple-stream?)]}]
 [simple-stream-unfold {[max (->i ([st simple-stream?])
                                  (values [r1 (st) (equal?/c (simple-stream-first st))]
                                          [r2 simple-stream?]))]
                        [types (-> simple-stream? (values any/c simple-stream?))]}]
 [simple-stream-get {[max (->i ([st simple-stream?]
                                [i exact-nonnegative-integer?])
                               [result (st i)
                                       (equal?/c (for/fold ([current-st st]
                                                            #:result (simple-stream-first current-st))
                                                           ([_ (in-range i)])
                                                   ((simple-stream-rest current-st))))])]
                     [types (-> simple-stream? exact-nonnegative-integer? any/c)]}]
 [simple-stream-take {[max (->i ([st simple-stream?]
                                 [n exact-nonnegative-integer?])
                                [result (st n)
                                        (and/c list?
                                               (equal?/c
                                                (for/fold ([lst '()]
                                                           [current-st st]
                                                           #:result (reverse lst))
                                                          ([_ (in-range n)])
                                                  (values (cons (simple-stream-first current-st) lst)
                                                          ((simple-stream-rest current-st))))))])]
                      [types (-> simple-stream? exact-nonnegative-integer? (listof any/c))]}])

(provide (struct-out simple-stream)

         simple-stream/c
         simple-streamof
         simple-stream/dc
         simple-stream/dc*)

;; A simple-stream is a cons of a value and a thunk that computes the next value when applied
(struct simple-stream (first rest) #:transparent)


(define/ctc-helper (simple-stream/c first/c rest/c)
  (struct/c simple-stream first/c rest/c))

(define/ctc-helper (simple-streamof el/c)
  (letrec ([this-ctc (simple-stream/c el/c (-> (recursive-contract this-ctc #:chaperone)))])
    this-ctc))

;; elem-contract? (elem -> elem-contract?) -> simple-stream-contract?
(define/ctc-helper (simple-stream/dc* first/c next/c-maker)
  (simple-stream/dc first/c
                    (λ (first)
                      (-> (simple-stream/dc* (next/c-maker first)
                                             next/c-maker)))))

;; Custom projections aren't supported
#;(define/ctc-helper (simple-stream/dc first/c make-rest/c)
  (define first/c-proj (get/build-late-neg-projection first/c))
  (make-contract
   #:name 'simple-stream/dc
   #:late-neg-projection
   (λ (blame)
     (λ (val neg-party)
       (unless (simple-stream? val)
         (raise-blame-error
          blame #:missing-party neg-party
          val
          '(expected "a simple-stream" given: "~e")
          val))

       (define first/checked ((first/c-proj blame) (simple-stream-first val) neg-party))
       (define rest/c (make-rest/c first/checked))
       (define rest/c-proj (get/build-late-neg-projection rest/c))
       (simple-stream first/checked
                      ((rest/c-proj blame) (simple-stream-rest val)
                                           neg-party))))))

;; elem-contract? (elem -> (-> simple-stream-contract?)) -> simple-stream-contract?
(define/ctc-helper (simple-stream/dc first/c make-rest/c)
  (struct/dc simple-stream
             [first first/c]
             [rest (first) (make-rest/c first)]))

;;--------------------------------------------------------------------------------------------------

(define (make-simple-stream hd thunk)
  (simple-stream hd thunk))

;; `simple-stream-unfold st` Destruct a simple-stream `st` into its first value and the new simple-stream produced by de-thunking the tail
(define (simple-stream-unfold st)
  (values (simple-stream-first st) ((simple-stream-rest st))))

;; `simple-stream-get st i` Get the `i`-th element from the simple-stream `st`
(define (simple-stream-get st i)
  (define-values (hd tl) (simple-stream-unfold st))
  (cond [(= i 0) hd]
        [else    (simple-stream-get tl (sub1 i))]))

;; `simple-stream-take st n` Collect the first `n` elements of the simple-stream `st`.
(define (simple-stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (simple-stream-unfold st))
              (cons hd (simple-stream-take tl (sub1 n)))]))

(module+ test
  (require rackunit)
  ;; the simplest stream, which always produces 1
  (define (ones)
    (make-simple-stream 1 ones))
  ;; `simple-stream-unfold`
  (define-values (first-ones rest-ones) (simple-stream-unfold (ones)))
  (check-equal? 1 first-ones)
  (define-values (first-ones-2 _rest-ones-2) (simple-stream-unfold rest-ones))
  (check-equal? 1 first-ones-2)
  ;; `simple-stream-get`
  (check-equal? 1 (simple-stream-get (ones) 0))
  (check-equal? 1 (simple-stream-get (ones) 4))
  ;; `simple-stream-take`
  (check-equal? '() (simple-stream-take (ones) 0))
  (check-equal? '(1 1 1 1 1) (simple-stream-take (ones) 5))

  ;; a slightly more complicated stream, which produces powers of 2
  (define powers-of-2
    (let next ([n 1])
      (make-simple-stream n (λ () (next (* n 2))))))
  ;; `simple-stream-unfold`
  (define-values (first-pow rest-pow) (simple-stream-unfold powers-of-2))
  (check-equal? 1 first-pow)
  (define-values (first-pow-2 rest-pow-2) (simple-stream-unfold rest-pow))
  (check-equal? 2 first-pow-2)
  (define-values (first-pow-3 _rest-pow-3) (simple-stream-unfold rest-pow-2))
  (check-equal? 4 first-pow-3)
  ;; `simple-stream-get`
  (check-equal? 1 (simple-stream-get powers-of-2 0))
  (check-equal? 2 (simple-stream-get powers-of-2 1))
  (check-equal? 1024 (simple-stream-get powers-of-2 10))
  ;; `simple-stream-take`
  (check-equal? '() (simple-stream-take powers-of-2 0))
  (check-equal? '(1 2) (simple-stream-take powers-of-2 2))
  (check-equal? '(1 2 4 8 16) (simple-stream-take powers-of-2 5))
  (check-equal? '(4 8 16 32 64) (simple-stream-take rest-pow-2 5)))
