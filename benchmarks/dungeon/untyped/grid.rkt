#lang racket

(require
  "../base/un-types.rkt"
  require-typed-check
  ;math/array ;; TODO it'd be nice to use this
 racket/contract
 (only-in "../../../ctcs/common.rkt" or-#f/c)
 "../../../ctcs/configurable.rkt"
 "../../../ctcs/precision-config.rkt"
)
(require (only-in "cell.rkt"
;;   char->cell%
;;   void-cell%
  cell%?
  class-equal?
))
(require/configurable-contract "cell.rkt" void-cell% char->cell% )

(provide/configurable-contract
 [array-set! ([max (->i ([g (arrayof cell%?)]
                         [p array-coord?]
                         [v cell%?])
                        [result void?]
                        #:post (g p v) (equal? v (grid-ref g p)))]
              [types ((arrayof cell%?) array-coord? cell%? . -> . void?)])]
 [build-array ([max (->i ([p array-coord?]
                          [f (array-coord? . -> . cell%?)])
                         [result (p)
                                 (and/c (arrayof cell%?)
                                        (array-size=/c p))]
                         #:post (p f result)
                         (for*/and ([x (in-range (vector-ref p 0))]
                                    [y (in-range (vector-ref p 1))])
                           (define xy (vector x y))
                           (equal? (f xy)
                                   (grid-ref result xy))))]
               [types (array-coord? (array-coord? . -> . cell%?) . -> . (arrayof cell%?))])]
 [parse-grid ([max ((listof string?) . -> . grid?)]
              [types ((listof string?) . -> . grid?)])]
 [show-grid ([max (grid? . -> . string?)]
             [types (grid? . -> . string?)])]
 [grid-height ([max (->i ([g grid?])
                         [result (g) (and/c index?
                                            (curry equal? (vector-length g)))])]
               [types (grid? . -> . index?)])]
 [grid-width ([max (->i ([g grid?])
                        [result (g) (and/c index?
                                           (curry equal? 
                                                  (vector-length (vector-ref g 0))))])]
              [types (grid? . -> . index?)])]
 [within-grid? ([max (->i ([g grid?]
                           [pos array-coord?])
                          [result 
                           (g pos)
                           (curry equal? 
                                  (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
                                       (<= 0 (vector-ref pos 1) (sub1 (grid-width  g)))))])]
                [types (grid? array-coord? . -> . boolean?)])]
 [grid-ref ([max (->i ([g grid?]
                       [pos array-coord?])
                      [result 
                       (g pos)
                       (or-#f/c
                        (and/c cell%?
                               (curry equal?
                                      (when (within-grid? g pos)
                                        (vector-ref (vector-ref g (vector-ref pos 0))
                                                    (vector-ref pos 1))))))])]
            [types (grid? array-coord? . -> . (or-#f/c cell%?))])]
 [left ([max (and/c direction?
                    (->i ([pos array-coord?])
                         ([n exact-nonnegative-integer?])
                         [result
                          (pos n)
                          (vector/c (vector-ref pos 0)
                                    (max (- (vector-ref pos 1) (if (unsupplied-arg? n) 1 n)) 0))]))]
        [types direction?])]
 [right ([max (and/c direction?
                     (->i ([pos array-coord?])
                          ([n exact-nonnegative-integer?])
                          [result
                           (pos n)
                           (vector/c (vector-ref pos 0)
                                     (max (+ (vector-ref pos 1) (if (unsupplied-arg? n) 1 n)) 0))]))]
         [types direction?])]
 [up ([max (and/c direction?
                  (->i ([pos array-coord?])
                       ([n exact-nonnegative-integer?])
                       [result (pos n) (vector/c (max (- (vector-ref pos 0)
                                                         (if (unsupplied-arg? n) 1 n))
                                                      0)
                                                 (vector-ref pos 1))]))]
      [types direction?])]
 [down ([max (and/c direction?
                    (->i ([pos array-coord?])
                         ([n exact-nonnegative-integer?])
                         [result (pos n) (vector/c (max (+ (vector-ref pos 0)
                                                           (if (unsupplied-arg? n) 1 n))
                                                        0)
                                                   (vector-ref pos 1))]))]
        [types direction?])])

(provide
;;   left
;;   right
;;   up
;;   down
;;   grid-ref
;;   grid-height
;;   grid-width
;;   show-grid
;;   array-set!
;;   build-array
  array-coord?
  direction?
  arrayof
  grid?
;; within-grid?
  within-grid/c
)

;; =============================================================================

(define/ctc-helper array-coord? (vector/c index? index?))
(define/ctc-helper (arrayof val-ctc)
  (vectorof (vectorof val-ctc)))

(define (array-set! g p v)
  (vector-set! (vector-ref g (vector-ref p 0)) (vector-ref p 1) v))

(define/ctc-helper ((array-size=/c dims) array)
  (match-define (vector x y) dims)
  (and (= (vector-length array) x)
       (if (not (zero? x))
           (= (vector-length (vector-ref array 0)) y)
           #t)))

(define (build-array p f)
  (for/vector ([x (in-range (vector-ref p 0))])
    (for/vector ([y (in-range (vector-ref p 1))])
      (f (vector (assert x index?) (assert y index?))))))
  ;(build-array p f)))

;; a Grid is a math/array Mutable-Array of cell%
;; (mutability is required for dungeon generation)
(define/ctc-helper grid? (arrayof cell%?))

;; parses a list of strings into a grid, based on the printed representation
;; of each cell
(define (parse-grid los)
  (for/vector
              ; #:shape (vector (length los)
              ;                (apply max (map string-length los)))
              ;#:fill (new void-cell%)
              ([s (in-list los)])
            (for/vector
               ([c (in-string s)])
     (new (char->cell% c)))))

(define (show-grid g)
  (with-output-to-string
    (lambda ()
      (for ([r (in-vector g)])
        (for ([c (in-vector r)])
          (display (send c show)))
        (newline)))))

(define (grid-height g)
  (vector-length g))

(define (grid-width g)
  (vector-length (vector-ref g 0)))

;; lltodo: can be more precise
(define (within-grid? g pos)
  (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
       (<= 0 (vector-ref pos 1) (sub1 (grid-width  g)))))

(define/ctc-helper ((within-grid/c g) pos)
  (within-grid? g pos))



(define (grid-ref g pos)
  (and (within-grid? g pos)
       (vector-ref (vector-ref g (vector-ref pos 0)) (vector-ref pos 1))))

(define/ctc-helper direction? (->* (array-coord?) [index?]
                        array-coord?))

(define (left pos [n 1])
  (vector (vector-ref pos 0)
          (max (- (vector-ref pos 1) n) 0)))

(define (right pos [n 1])
  (vector (vector-ref pos 0)
          (max (+ (vector-ref pos 1) n) 0)))

(define (up pos [n 1])
  (vector (max (- (vector-ref pos 0) n) 0)
          (vector-ref pos 1)))

(define (down pos [n 1])
  (vector (max (+ (vector-ref pos 0) n) 0)
          (vector-ref pos 1)))


(module+ test
  (require rackunit)


  (define (parse-and-show los) (show-grid (parse-grid los)))

  (define (render-grid g) (string-join g "\n" #:after-last "\n"))

  (define g1
    '(" "))
  (check-equal? (parse-and-show g1) " \n")

  (define g2
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "**********"))
  (check-equal? (parse-and-show g2) (render-grid g2))

  (define g3 ; padding should work
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "*****"))
  (define g3*
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "*****....."))
  (check-equal? (parse-and-show g3) (render-grid g3*))

  (define g2* (parse-grid g2))
  (check-true (within-grid? g2* '#(0 0)))
  (check-true (within-grid? g2* '#(0 1)))
  (check-true (within-grid? g2* '#(1 0)))
  (check-true (within-grid? g2* '#(4 4)))
  (check-false (within-grid? g2* '#(0 10)))
  (check-false (within-grid? g2* '#(5 0)))
  (check-false (within-grid? g2* '#(5 10)))
  )
