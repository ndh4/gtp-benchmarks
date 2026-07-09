#lang racket

(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt")

(provide make-simple-stream
         simple-stream-unfold
         simple-stream-get
         simple-stream-take)

(provide (struct-out simple-stream)
         simple-stream/c
         simple-streamof
         simple-stream/dc
         simple-stream/dc*)

(struct simple-stream (first rest) #:transparent)

(define/ctc-helper
 (simple-stream/c first/c rest/c)
 (struct/c simple-stream first/c rest/c))

(define/ctc-helper
 (simple-streamof el/c)
 (struct
  make-sso-contract
  ()
  #:property
  prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (λ (c)
     (string->symbol (format "(simple-streamof ~a)" (contract-name el/c))))
   #:late-neg-projection
   (λ (c)
     (λ (blame)
       (λ (val missing-party)
         (cond
          ((simple-stream? val)
           (chaperone-struct
            val
            struct:simple-stream
            simple-stream-first
            (λ (orig-struct first-val)
              (((contract-late-neg-projection el/c) blame)
               first-val
               missing-party))
            simple-stream-rest
            (λ (orig-struct rest-val)
              (((contract-late-neg-projection (-> c)) blame)
               rest-val
               missing-party))))
          (else
           (raise-blame-error
            blame
            #:missing-party
            missing-party
            val
            '(expected "simple-stream?" given: "~e")
            val))))))
   #:generate
   (λ (c)
     (λ (fuel)
       (define el-gen (contract-random-generate/choose el/c fuel))
       (define (stream-gen seed)
         (random-seed seed)
         (rand-seed seed)
         (define elem (el-gen))
         (define new-seed (random (sub1 (expt 2 31)) my-generator))
         (define (get-rest) (stream-gen new-seed))
         (simple-stream elem get-rest))
       (thunk (stream-gen (random (sub1 (expt 2 31)))))))))
 (make-sso-contract))

(define/ctc-helper
 (simple-stream/dc* first/c next/c-maker)
 (simple-stream/dc
  first/c
  (λ (first) (-> (simple-stream/dc* (next/c-maker first) next/c-maker)))))

(define/ctc-helper
 (simple-stream/dc first/c make-rest/c)
 (struct/dc simple-stream (first first/c) (rest (first) (make-rest/c first))))

(define/contract
 (make-simple-stream hd thunk)
 (configurable-ctc
  (max
   (->i
    ((hd any/c) (thunk (-> (simple-streamof any/c))))
    (result (hd thunk) (simple-stream/c (equal?/c hd) (equal?/c thunk)))))
  (types (-> any/c (-> (simple-streamof any/c)) (simple-streamof any/c))))
 (simple-stream hd thunk))

(define/contract
 (simple-stream-unfold st)
 (configurable-ctc
  (max
   (->i
    ((st (simple-streamof any/c)))
    (values
     (r1 (st) (equal?/c (simple-stream-first st)))
     (r2 (simple-streamof any/c)))))
  (types (-> (simple-streamof any/c) (values any/c (simple-streamof any/c)))))
 (values (simple-stream-first st) ((simple-stream-rest st))))

(define/contract
 (simple-stream-get st i)
 (configurable-ctc
  (max
   (->i
    ((st (simple-streamof any/c)) (i exact-nonnegative-integer?/small-gen))
    (result
     (st i)
     (equal?/c
      (for/fold
       ((current-st st) #:result (simple-stream-first current-st))
       ((_ (in-range i)))
       ((simple-stream-rest current-st)))))))
  (types
   (-> (simple-streamof any/c) exact-nonnegative-integer?/small-gen any/c)))
 (define-values (hd tl) (simple-stream-unfold st))
 (cond ((= i 0) hd) (else (simple-stream-get tl (sub1 i)))))

(define/contract
 (simple-stream-take st n)
 (configurable-ctc
  (max
   (->i
    ((st (simple-streamof any/c)) (n exact-nonnegative-integer?/small-gen))
    (result
     (st n)
     (and/c
      list?
      (equal?/c
       (for/fold
        ((lst '()) (current-st st) #:result (reverse lst))
        ((_ (in-range n)))
        (values
         (cons (simple-stream-first current-st) lst)
         ((simple-stream-rest current-st)))))))))
  (types
   (->
    (simple-streamof any/c)
    exact-nonnegative-integer?/small-gen
    (listof any/c))))
 (cond
  ((= n 0) '())
  (else
   (define-values (hd tl) (simple-stream-unfold st))
   (cons hd (simple-stream-take tl (sub1 n))))))

(module+
 test
 (require rackunit)
 (define (ones) (make-simple-stream 1 ones))
 (define-values (first-ones rest-ones) (simple-stream-unfold (ones)))
 (check-equal? 1 first-ones)
 (define-values (first-ones-2 _rest-ones-2) (simple-stream-unfold rest-ones))
 (check-equal? 1 first-ones-2)
 (check-equal? 1 (simple-stream-get (ones) 0))
 (check-equal? 1 (simple-stream-get (ones) 4))
 (check-equal? '() (simple-stream-take (ones) 0))
 (check-equal? '(1 1 1 1 1) (simple-stream-take (ones) 5))
 (define powers-of-2
   (let next ((n 1)) (make-simple-stream n (λ () (next (* n 2))))))
 (define-values (first-pow rest-pow) (simple-stream-unfold powers-of-2))
 (check-equal? 1 first-pow)
 (define-values (first-pow-2 rest-pow-2) (simple-stream-unfold rest-pow))
 (check-equal? 2 first-pow-2)
 (define-values (first-pow-3 _rest-pow-3) (simple-stream-unfold rest-pow-2))
 (check-equal? 4 first-pow-3)
 (check-equal? 1 (simple-stream-get powers-of-2 0))
 (check-equal? 2 (simple-stream-get powers-of-2 1))
 (check-equal? 1024 (simple-stream-get powers-of-2 10))
 (check-equal? '() (simple-stream-take powers-of-2 0))
 (check-equal? '(1 2) (simple-stream-take powers-of-2 2))
 (check-equal? '(1 2 4 8 16) (simple-stream-take powers-of-2 5))
 (check-equal? '(4 8 16 32 64) (simple-stream-take rest-pow-2 5)))

