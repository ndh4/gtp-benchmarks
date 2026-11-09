#lang racket

(require (except-in "data.rkt" posn=?)
         (except-in "cut-tail.rkt" cut-tail)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(require (only-in "cut-tail.rkt" cut-tail))

(require (only-in "data.rkt" posn=?))

(provide next-head snake-slither snake-grow)

(define/contract
 (next-head seg dir)
 (configurable-ctc
  (max
   (->i
    ((seg posn-type?) (dir snake-dir?))
    (result
     (seg dir)
     (posn=?/c
      (match*
       (seg dir)
       (((posn x y) "right") (posn (add1 x) y))
       (((posn x y) "left") (posn (sub1 x) y))
       (((posn x y) "down") (posn x (sub1 y)))
       (((posn x y) "up") (posn x (add1 y))))))))
  (types (-> posn-type? string? posn-type?)))
 (cond
  ((equal? "right" dir) (posn (add1 (posn-x seg)) (posn-y seg)))
  ((equal? "left" dir) (posn (sub1 (posn-x seg)) (posn-y seg)))
  ((equal? "down" dir) (posn (posn-x seg) (sub1 (posn-y seg))))
  (else (posn (posn-x seg) (add1 (posn-y seg))))))

(define/contract
 (snake-slither snk)
 (configurable-ctc
  (max
   (->i
    ((snk snake-type?))
    (result
     (snk)
     (and/c
      (match
       snk
       ((snake d (and segs (cons segs/h segs/t)))
        (snake/c
         d
         (cons/c
          (posn=?/c (next-head segs/h d))
          (snake-segs=?/c (drop-right segs 1)))))
       (_ #f))))))
  (types (-> snake-type? snake-type?)))
 (let ((d (snake-dir snk)))
   (snake
    d
    (cons (next-head (car (snake-segs snk)) d) (cut-tail (snake-segs snk))))))

(define/contract
 (snake-grow snk)
 (configurable-ctc
  (max
   (->i
    ((snk snake-type?))
    (result
     (snk)
     (and/c
      (match
       snk
       ((snake d (and segs (cons segs/h segs/t)))
        (snake/c
         d
         (cons/c (posn=?/c (next-head segs/h d)) (snake-segs=?/c segs)))))))))
  (types (-> snake-type? snake-type?)))
 (let ((d (snake-dir snk)))
   (snake d (cons (next-head (car (snake-segs snk)) d) (snake-segs snk)))))

(module+
 test
 (require rackunit)
 (check-equal? (next-head (posn 1 1) "right") (posn 2 1))
 (check-equal? (next-head (posn 1 1) "left") (posn 0 1))
 (check-equal? (next-head (posn 1 1) "down") (posn 1 0))
 (check-equal? (next-head (posn 1 1) "up") (posn 1 2))
 (check-equal? (next-head (posn 0 0) "right") (posn 1 0))
 (check-equal? (next-head (posn 10 10) "left") (posn 9 10))
 (check-equal? (next-head (posn 5 5) "invalid") (posn 5 6))
 (check-equal?
  (snake-grow (snake "right" (list (posn 1 1))))
  (snake "right" (list (posn 2 1) (posn 1 1))))
 (check-equal?
  (snake-grow (snake "left" (list (posn 5 5) (posn 4 5))))
  (snake "left" (list (posn 4 5) (posn 5 5) (posn 4 5))))
 (check-equal?
  (snake-slither (snake "right" (list (posn 1 1))))
  (snake "right" (list (posn 2 1))))
 (check-equal?
  (snake-grow (snake "right" (list (posn 1 1) (posn 1 0))))
  (snake "right" (list (posn 2 1) (posn 1 1) (posn 1 0)))))

