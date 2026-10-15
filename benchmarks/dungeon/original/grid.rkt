#lang racket

(require "../base/un-types.rkt"
         racket/contract
         (only-in
          "../../../ctcs/common.rkt"
          make-dependent-contract
          array-of-contract
          or-#f/c
          stringof
          my-vector/c
          my-vectorof
          contract-with-generator
          get-random-element)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt")

(require (only-in "cell.rkt" cell%? class-equal? registered-char?))

(require (only-in "cell.rkt" void-cell% char->cell% chars->cell%s))

(provide array-set!
         build-array
         parse-grid
         show-grid
         grid-height
         grid-width
         within-grid?
         grid-ref
         left
         right
         up
         down)

(provide array-coord?
         direction?
         arrayof
         grid?
         within-grid/c
         generate-pos-within-grid)

(define/ctc-helper array-coord? (my-vector/c index/c index/c))

(define/ctc-helper array-size? (my-vector/c size/c size/c))

(define/ctc-helper
 (arrayof val-ctc)
 (array-of-contract (coerce-contract 'array-of val-ctc)))

(define/ctc-helper grid? (arrayof cell%?))

(define/ctc-helper
 (generate-pos-within-grid grid fuel)
 (vector (random (grid-height grid)) (random (grid-width grid))))

(define/ctc-helper
 array-set!-args/c*
 (make-dependent-contract
  grid?
  (lambda (grid) (and/c array-coord? (within-grid/c grid)))
  generate-pos-within-grid))

(define/ctc-helper array-set!-grid-ctc (first array-set!-args/c*))

(define/ctc-helper array-set!-pos-ctc (second array-set!-args/c*))

(define/ctc-helper array-set!-ctc-reset!? (third array-set!-args/c*))

(define/contract
 (array-set! g p v)
 (configurable-ctc
  (max
   (->i
    ((g array-set!-grid-ctc) (p array-set!-pos-ctc) (v cell%?))
    (result void?)
    #:post
    (g p v)
    (and (array-set!-ctc-reset!?) (equal? v (grid-ref g p)))))
  (types (-> grid? array-coord? cell%? void?)))
 (vector-set! (vector-ref g (vector-ref p 0)) (vector-ref p 1) v))

(define/ctc-helper
 ((array-size=/c dims) array)
 (match-define (vector x y) dims)
 (and (= (vector-length array) x)
      (if (not (zero? x)) (= (vector-length (vector-ref array 0)) y) #t)))

(define/contract
 (build-array p f)
 (configurable-ctc
  (max
   (->i
    ((p array-size?) (f (-> array-coord? cell%?)))
    (result (p) (and/c grid? (array-size=/c p)))))
  (types (-> array-size? (-> array-coord? cell%?) grid?)))
 (for/vector
  ((x (in-range (vector-ref p 0))))
  (for/vector
   ((y (in-range (vector-ref p 1))))
   (f (vector (assert x index?) (assert y index?))))))

(define/contract
 (parse-grid los)
 (configurable-ctc
  (max
   (-> (non-empty-listof (stringof registered-char? #:non-empty? #t)) grid?))
  (types (-> (listof string?) grid?)))
 (for/vector
  ((s (in-list los)))
  (for/vector ((c (in-string s))) (new (char->cell% c)))))

(define/contract
 (show-grid g)
 (configurable-ctc (max (-> grid? string?)) (types (-> grid? string?)))
 (with-output-to-string
  (lambda ()
    (for
     ((r (in-vector g)))
     (for ((c (in-vector r))) (display (send c show)))
     (newline)))))

(define/contract
 (grid-height g)
 (configurable-ctc
  (max
   (->i
    ((g grid?))
    (result (g) (and/c index/c (curry equal? (vector-length g))))))
  (types (-> grid? index/c)))
 (vector-length g))

(define/contract
 (grid-width g)
 (configurable-ctc
  (max
   (->i
    ((g grid?))
    (result
     (g)
     (and/c index/c (curry equal? (vector-length (vector-ref g 0)))))))
  (types (-> grid? index/c)))
 (vector-length (vector-ref g 0)))

(define/contract
 (within-grid? g pos)
 (configurable-ctc
  (max
   (->i
    ((g grid?) (pos array-coord?))
    (result
     (g pos)
     (curry
      equal?
      (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
           (<= 0 (vector-ref pos 1) (sub1 (grid-width g))))))))
  (types (-> grid? array-coord? boolean?)))
 (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
      (<= 0 (vector-ref pos 1) (sub1 (grid-width g)))))

(define/ctc-helper ((within-grid/c g) pos) (within-grid? g pos))

(define/contract
 (grid-ref g pos)
 (configurable-ctc
  (max
   (->i
    ((g grid?) (pos array-coord?))
    (result
     (g pos)
     (or-#f/c
      (and/c
       cell%?
       (curry
        equal?
        (when (within-grid? g pos)
          (vector-ref
           (vector-ref g (vector-ref pos 0))
           (vector-ref pos 1)))))))))
  (types (-> grid? array-coord? (or-#f/c cell%?))))
 (and (within-grid? g pos)
      (vector-ref (vector-ref g (vector-ref pos 0)) (vector-ref pos 1))))

(define/ctc-helper
 direction?
 (contract-with-generator
  (->* (array-coord?) (index/c) array-coord?)
  (lambda (fuel) (get-random-element (list left right up down)))))

(define/contract
 (left pos (n 1))
 (configurable-ctc
  (max
   (and/c
    direction?
    (->i
     ((pos array-coord?))
     ((n exact-nonnegative-integer?))
     (result
      (pos n)
      (my-vector/c
       (vector-ref pos 0)
       (max (- (vector-ref pos 1) (if (unsupplied-arg? n) 1 n)) 0))))))
  (types direction?))
 (vector (vector-ref pos 0) (max (- (vector-ref pos 1) n) 0)))

(define/contract
 (right pos (n 1))
 (configurable-ctc
  (max
   (and/c
    direction?
    (->i
     ((pos array-coord?))
     ((n exact-nonnegative-integer?))
     (result
      (pos n)
      (my-vector/c
       (vector-ref pos 0)
       (max (+ (vector-ref pos 1) (if (unsupplied-arg? n) 1 n)) 0))))))
  (types direction?))
 (vector (vector-ref pos 0) (max (+ (vector-ref pos 1) n) 0)))

(define/contract
 (up pos (n 1))
 (configurable-ctc
  (max
   (and/c
    direction?
    (->i
     ((pos array-coord?))
     ((n exact-nonnegative-integer?))
     (result
      (pos n)
      (my-vector/c
       (max (- (vector-ref pos 0) (if (unsupplied-arg? n) 1 n)) 0)
       (vector-ref pos 1))))))
  (types direction?))
 (vector (max (- (vector-ref pos 0) n) 0) (vector-ref pos 1)))

(define/contract
 (down pos (n 1))
 (configurable-ctc
  (max
   (and/c
    direction?
    (->i
     ((pos array-coord?))
     ((n exact-nonnegative-integer?))
     (result
      (pos n)
      (my-vector/c
       (max (+ (vector-ref pos 0) (if (unsupplied-arg? n) 1 n)) 0)
       (vector-ref pos 1))))))
  (types direction?))
 (vector (max (+ (vector-ref pos 0) n) 0) (vector-ref pos 1)))

(module+
 test
 (require "cell.rkt")
 (require rackunit)
 (check-equal? (left (vector 3 3)) (vector 3 2))
 (check-equal? (left (vector 3 0)) (vector 3 0))
 (check-equal? (left (vector 3 3) 2) (vector 3 1))
 (check-equal? (left (vector 3 3) 4) (vector 3 0))
 (check-equal? (right (vector 3 3)) (vector 3 4))
 (check-equal? (right (vector 3 3) 5) (vector 3 8))
 (check-equal? (up (vector 3 3)) (vector 2 3))
 (check-equal? (up (vector 0 3)) (vector 0 3))
 (check-equal? (up (vector 3 3) 2) (vector 1 3))
 (check-equal? (up (vector 3 3) 4) (vector 0 3))
 (check-equal? (down (vector 3 3)) (vector 4 3))
 (check-equal? (down (vector 3 3) 5) (vector 8 3))
 (define (parse-and-show los) (show-grid (parse-grid los)))
 (define (render-grid g) (string-join g "\n" #:after-last "\n"))
 (define g1 '(" "))
 (check-equal? (parse-and-show g1) " \n")
 (define g2
   '("XXXXX'XXXX" "X. #     X" "_    *  #X" "X        X" "XXXXXXXXXX"))
 (check-equal? (parse-and-show g2) (render-grid g2))
 (define g2* (parse-grid g2))
 (check-true (within-grid? g2* '#(0 0)))
 (check-true (within-grid? g2* '#(0 1)))
 (check-true (within-grid? g2* '#(1 0)))
 (check-true (within-grid? g2* '#(4 4)))
 (check-false (within-grid? g2* '#(0 10)))
 (check-false (within-grid? g2* '#(5 0)))
 (check-false (within-grid? g2* '#(5 10)))
 (check-equal? (grid-ref g2* '#(0 0)) (new wall%))
 (check-equal? (grid-ref g2* '#(2 0)) (new other-vertical-door%))
 (check-equal? (grid-ref g2* '#(0 5)) (new other-horizontal-door%))
 (check-equal? (grid-ref g2* '#(1 3)) (new pillar%))
 (check-equal? (grid-ref g2* '#(3 1)) (new empty-cell%))
 (check-equal? (grid-ref g2* '#(2 5)) (new cell%))
 (check-equal? (grid-ref g2* '#(1 1)) (new void-cell%))
 (check-equal? (grid-width g2*) 10)
 (check-equal? (grid-height g2*) 5)
 (define g3
   '("XXXXX-XXXX" "X. +     X" "|    *  +X" "X        X" "XXXXXXXXXX"))
 (check-equal? (parse-and-show g3) (render-grid g2))
 (define g3* (parse-grid g3))
 (check-equal? (grid-ref g3* '#(2 0)) (new vertical-door%))
 (check-equal? (grid-ref g3* '#(0 5)) (new horizontal-door%)))

