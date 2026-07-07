#lang racket

(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/common.rkt")

(provide posn=?)

(provide (struct-out posn))

(provide (struct-out snake)
         (struct-out world)
         posn?
         posn-type?
         posn/c
         snake-segs?
         snake-segs=?/c
         snake-dir?
         snake-type?
         snake/c
         snake=?/c
         posn=?/c
         world/c
         world-type?
         world=?/c
         food=?/c)

(struct posn (x y) #:transparent)

(define/ctc-helper
 posn-type?
 (flat-named-contract
  'posn-type?
  (λ (p) (match p ((posn (? integer?) (? integer?)) #t) (_ #f)))
  (λ (fuel)
    (define int-generator
      (contract-random-generate/choose integer? (sub1 fuel)))
    (thunk (posn (int-generator) (int-generator))))))

(define/ctc-helper
 ((posn=?/c p1) p2)
 (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))

(define/ctc-helper
 ((posn/c x/c y/c) x)
 (match
  x
  ((posn (? (and/c integer? x/c)) (? (and/c integer? y/c))) #t)
  (_ #f)))

(define/ctc-helper snake-segs? (listof posn-type?))

(define/ctc-helper nonempty-snake-segs? (and/c cons? snake-segs?))

(define/ctc-helper (snake-segs=?/c segs) (apply list/c (map posn=?/c segs)))

(define/ctc-helper food? posn-type?)

(define/ctc-helper food=?/c posn=?/c)

(define/ctc-helper snake-dir? (or/c "up" "down" "left" "right"))

(struct snake (dir segs) #:transparent)

(define/ctc-helper
 snake-type?
 (flat-named-contract
  'snake-type?
  (λ (s) (match s ((snake (? snake-dir?) (? nonempty-snake-segs?)) #t) (_ #f)))
  (λ (fuel)
    (define snake-dir?-generator
      (contract-random-generate/choose snake-dir? (sub1 fuel)))
    (define snake-segs?-generator
      (contract-random-generate/choose nonempty-snake-segs? (sub1 fuel)))
    (thunk (snake (snake-dir?-generator) (snake-segs?-generator))))))

(define/ctc-helper
 ((snake/c dir/c segs/c) s)
 (match
  s
  ((snake (? (and/c snake-dir? dir/c)) (? (and/c snake-segs? segs/c))) #t)
  (_ #f)))

(define/ctc-helper
 ((snake=?/c s1) s2)
 (match*
  (s1 s2)
  (((snake dir segs1) (snake dir segs2)) ((snake-segs=?/c segs1) segs2))
  ((_ _) #f)))

(struct world (snake food) #:transparent)

(define/ctc-helper
 (world/c snake/c food/c)
 (flat-named-contract
  (string->symbol
   (format "(world/c ~a ~a)" (contract-name snake/c) (contract-name food/c)))
  (λ (x) (match x ((world (? snake/c) (? food/c)) #t) (_ #f)))
  (λ (fuel)
    (define snake-generator
      (contract-random-generate/choose snake/c (sub1 fuel)))
    (define food-generator
      (contract-random-generate/choose food/c (sub1 fuel)))
    (thunk (world (snake-generator) (food-generator))))))

(define/ctc-helper
 ((world=?/c w1) w2)
 (match*
  (w1 w2)
  (((world snake1 food1) (world snake2 food2))
   (and ((snake=?/c snake1) snake2) ((food=?/c food1) food2)))
  ((_ _) #f)))

(define/ctc-helper world-type? (world/c snake-type? food?))

(define/contract
 (posn=? p1 p2)
 (configurable-ctc
  (max
   (->i
    ((p1 posn-type?) (p2 posn-type?))
    (result
     (p1 p2)
     (not
      (or (not (= (posn-x p1) (posn-x p2)))
          (not (= (posn-y p1) (posn-y p2))))))))
  (types (-> posn-type? posn-type? boolean?)))
 (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))

(module+
 test
 (require rackunit)
 (check-equal? (posn=? (posn 1 2) (posn 1 2)) #t)
 (check-equal? (posn=? (posn 1 2) (posn 2 2)) #f)
 (check-equal? (posn=? (posn 2 1) (posn 2 2)) #f))

