#lang racket

(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt")
(require "streams.rkt")

;;--------------------------------------------------------------------------------------------------

;; ll: All these configurable-ctc forms are commented out (unnecessary atm) because
;; main provides nothing!

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define/contract (count-from n)
  (configurable-ctc
   [max (->i ([n integer?])
             [result (n)
                     (simple-stream/dc* (and/c integer? (=/c n))
                                        (λ (last)
                                          (and/c integer? (=/c (add1 last)))))])]
   [types (-> number? (simple-streamof number?))])
  (make-simple-stream n (lambda () (count-from (add1 n)))))

(define/ctc-helper ((divisible-by/c divisor) x)
  (zero? (modulo x divisor)))

;; `sift n st` Filter all elements in `st` that are multiples of `n`.
;; Return a new simple-stream.
(define/contract (sift n st)
  (configurable-ctc
   [max (->i ([n natural?]
              [st (simple-streamof natural?)])
             [result (n)
                     (simple-streamof (and/c natural?
                                             (not/c (divisible-by/c n))))])]
   [types (-> natural? (simple-streamof natural?) (simple-streamof natural?))])
  (define-values (hd tl) (simple-stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-simple-stream hd (lambda () (sift n tl)))]))

(define/ctc-helper prime? (let () (local-require math/number-theory) prime?))

(define/ctc-helper (sieved-simple-stream-following/c sieved-n)
  (and/c (simple-streamof (and/c integer? (not/c (divisible-by/c sieved-n))))
         (simple-stream/dc any/c
                           (λ (first)
                             (-> (sieved-simple-stream-following/c first))))))

;; `sieve st` Sieve of Eratosthenes
(define/contract (sieve st)
  (configurable-ctc
   [max (->i ([st (simple-streamof natural?)])
             [result (st)
                     (let ([first (simple-stream-first st)])
                       (simple-stream/c (and/c natural? (=/c first))
                                        (-> (sieved-simple-stream-following/c first))))])]
   [types (-> (simple-streamof natural?) (simple-streamof natural?))])
  (define-values (hd tl) (simple-stream-unfold st))
  (make-simple-stream hd (lambda () (sieve (sift hd tl)))))

;; simple-stream of prime numbers
(define/contract primes
  (configurable-ctc
   [max (simple-streamof (and/c natural? prime?))]
   [types (simple-streamof natural?)])
  (sieve (count-from 2)))

(define/contract N-1
  (configurable-ctc
   [max (and/c natural? (=/c 20))]
   [types natural?])
  20)

(define/contract (main)
  (configurable-ctc
   [max (-> void?)]
   [types (-> void?)])
  (void (simple-stream-get primes N-1)))

(module+ test
  (require rackunit)
  ;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
  (define counter (count-from 2))
  (check-equal? 2 (simple-stream-get counter 0))
  (check-equal? 6 (simple-stream-get counter 4))
  (check-equal? '(2 3 4 5 6) (simple-stream-take counter 5))
  ;; `sift n st` Filter all elements in `st` that are multiples of `n`.
  (check-equal? '(2 4) (simple-stream-take (sift 3 counter) 2))
  (check-equal? '(2 4 5 7 8 10 11 13 14) (simple-stream-take (sift 3 counter) 9))
  (check-equal? '(3 5 7 9 11) (simple-stream-take (sift 2 counter) 5))
  (check-equal? '(2 3 4 6 7) (simple-stream-take (sift 5 counter) 5))
  ;; `sieve st` Sieve of Eratosthenes
  (check-equal? '(5 6 7 8 9 11 13) (simple-stream-take (sieve (count-from 5)) 7))
  (define multiples-of-two
    (let loop ([n 4])
      (make-simple-stream n (lambda () (loop (+ n 2))))))
  (check-equal? '(4 6 10 14 22 26 34) (simple-stream-take (sieve multiples-of-two) 7))
  ;; `primes` simple-stream of prime numbers
  (check-equal? '(2 3 5 7 11 13 17 19 23 29) (simple-stream-take primes 10))
)
