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
 (posn-type? p)
 (match p ((posn (? integer?) (? integer?)) #t) (_ #f)))

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

(define/ctc-helper (snake-segs=?/c segs) (apply list/c (map posn=?/c segs)))

(define/ctc-helper food? posn-type?)

(define/ctc-helper food=?/c posn=?/c)

(define/ctc-helper snake-dir? (or/c "up" "down" "left" "right"))

(struct snake (dir segs))

(define/ctc-helper
 (snake-type? s)
 (match s ((snake (? string?) (? snake-segs?)) #t) (_ #f)))

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

(struct world (snake food))

(define/ctc-helper
 ((world/c snake/c food/c) x)
 (match x ((world (? snake/c) (? food/c)) #t) (_ #f)))

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
    ((p1 posn?) (p2 posn?))
    (result (p1 p2) (match* (p1 p2) (((posn x y) (posn x y)) #t) ((_ _) #f)))))
  (types (-> posn? posn? boolean?)))
 (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))

#;(module+
 test
 (require rackunit)
 (check-equal? (posn=? (posn 1 2) (posn 1 2)) #t)
 (check-equal? (posn=? (posn 1 2) (posn 2 2)) #f)
 (check-equal? (posn=? (posn 2 1) (posn 2 2)) #f))

