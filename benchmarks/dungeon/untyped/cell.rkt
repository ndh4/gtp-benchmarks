#lang racket

(require racket/class
         "../base/un-types.rkt"
         racket/contract
         (only-in "../../../ctcs/common.rkt" class/c*)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt")

(require (only-in racket/function curry))

(require/configurable-contract "message-queue.rkt" enqueue-message!)

(require (only-in racket/dict dict-ref dict-set! dict-has-key?))

(provide/configurable-contract
 (cell% ((max (make-cell%/c-with self #\* equal?)) (types cell%/c)))
 (chars->cell%s ((max (hash/c char? cell%/c)) (types hash?)))
 (register-cell-type!
  ((max
    (->i
     ((c% cell%/c) (char char?))
     (result void?)
     #:post
     (c% char)
     (class-equal? (dict-ref chars->cell%s char void) c%)))
   (types (-> cell%/c char? void?))))
 (char->cell%
  ((max
    (->i
     ((char (and/c char? (curry dict-has-key? chars->cell%s))))
     (result
      (char)
      (and/c cell%/c (curry class-equal? (dict-ref chars->cell%s char))))))
   (types (-> char? cell%?))))
 (empty-cell%
  ((max
    (make-cell%/c-with
     self
     (or/c #\space (send (get-field occupant self) show))
     (not/c equal?)))
   (types cell%/c)))
 (void-cell% ((max (make-cell%/c-with self #\. equal?)) (types cell%/c)))
 (wall% ((max (make-cell%/c-with self #\X equal?)) (types cell%/c)))
 (double-bar? ((max boolean?) (types boolean?)))
 (door% ((max (make-cell%/c-with self #\* (not/c equal?))) (types cell%/c)))
 (vertical-door%
  ((max
    (make-cell%/c-with
     self
     (or/c #\_ (send (get-field occupant self) show))
     (not/c equal?)))
   (types cell%/c)))
 (other-vertical-door%
  ((max
    (make-cell%/c-with
     self
     (or/c #\_ (send (get-field occupant self) show))
     (not/c equal?)))
   (types cell%/c)))
 (horizontal-door%
  ((max
    (make-cell%/c-with
     self
     (or/c #\' (send (get-field occupant self) show))
     (not/c equal?)))
   (types cell%/c)))
 (other-horizontal-door%
  ((max
    (make-cell%/c-with
     self
     (or/c #\' (send (get-field occupant self) show))
     (not/c equal?)))
   (types cell%/c))))

(provide cell%? cell%/c class-equal?)

(define-syntax-rule/ctc-helper
 (make-cell%/c-with self-id show-char free?/occupant-comparer)
 (class/c*
  (init-field/all (items list?) (occupant any/c))
  (all (open (->m void?)) (close (->m void?)) (free? (->m boolean?)))
  (inherit+super (show (->i ((self-id any/c)) (result (self-id) show-char))))
  (override (show (->m char?)))
  (free?
   (->i
    ((self-id any/c))
    (result
     (self-id)
     (curry free?/occupant-comparer (get-field occupant self-id)))))))

(define/ctc-helper cell%/c (make-cell%/c-with self any/c (λ x #t)))

(define/ctc-helper cell%? (instanceof/c cell%/c))

(define cell%
  (class object%
    (inspect #f)
    (init-field (items '()) (occupant #f))
    (define/public (free?) #f)
    (define/public (show) #\*)
    (define/public (open) (enqueue-message! "Can't open that."))
    (define/public (close) (enqueue-message! "Can't close that."))
    (super-new)))

(define chars->cell%s (make-hash))

(define/ctc-helper
 (class-equal? a% b%)
 (and (subclass? a% b%) (subclass? b% a%)))

(define (register-cell-type! c% char) (dict-set! chars->cell%s char c%))

(define (char->cell% char) (dict-ref chars->cell%s char))

(register-cell-type! cell% #\*)

(define empty-cell%
  (class cell%
    (inspect #f)
    (inherit-field occupant)
    (define/override (free?) (not occupant))
    (define/override
     (show)
     (if occupant (send (or occupant (raise-user-error 'show)) show) #\space))
    (super-new)))

(register-cell-type! empty-cell% #\space)

(define void-cell%
  (class cell% (inspect #f) (define/override (show) #\.) (super-new)))

(register-cell-type! void-cell% #\.)

(define wall%
  (class cell% (inspect #f) (define/override (show) #\X) (super-new)))

(register-cell-type! wall% #\X)

(define double-bar? #t)

(define-syntax-rule
 (define-wall name single-bar double-bar)
 (begin
   (define name
     (class wall%
       (inspect #f)
       (define/override (show) (if double-bar? double-bar single-bar))
       (super-new)))
   (register-cell-type! name single-bar)
   (register-cell-type! name double-bar)
   (provide/configurable-contract
    (name
     ((max
       (make-cell%/c-with self (if double-bar? double-bar single-bar) equal?))
      (types cell%/c))))))

(define-wall pillar% #\+ #\#)

(define-wall vertical-wall% #\│ #\║)

(define-wall horizontal-wall% #\─ #\═)

(define-wall four-corner-wall% #\┼ #\╬)

(define-wall north-east-wall% #\┐ #\╗)

(define-wall north-west-wall% #\┌ #\╔)

(define-wall south-east-wall% #\┘ #\╝)

(define-wall south-west-wall% #\└ #\╚)

(define-wall north-tee-wall% #\┬ #\╦)

(define-wall south-tee-wall% #\┴ #\╩)

(define-wall east-tee-wall% #\┤ #\╣)

(define-wall west-tee-wall% #\├ #\╠)

(define door%
  (class cell%
    (inspect #f)
    (inherit-field occupant)
    (define/override (free?) (and (not occupant)))
    (define/override
     (open)
     (if #t (enqueue-message! "The door is already open.") (void)))
    (define/override
     (close)
     (if #t (void) (enqueue-message! "The door is already closed.")))
    (super-new)))

(define vertical-door%
  (class door%
    (inspect #f)
    (inherit-field occupant)
    (define/override
     (show)
     (if #t
       (if occupant (send (or occupant (raise-user-error 'vdoor)) show) #\_)
       #\|))
    (super-new)))

(register-cell-type! vertical-door% #\|)

(define other-vertical-door% (class vertical-door% (inspect #f) (super-new)))

(register-cell-type! other-vertical-door% #\_)

(define horizontal-door%
  (class door%
    (inspect #f)
    (inherit-field occupant)
    (define/override
     (show)
     (if #t
       (if occupant (send (or occupant (raise-user-error 'hdoor)) show) #\')
       #\-))
    (super-new)))

(register-cell-type! horizontal-door% #\-)

(define other-horizontal-door%
  (class horizontal-door% (inspect #f) (super-new)))

(register-cell-type! other-horizontal-door% #\')

