#lang racket

(require racket/class
         "../base/un-types.rkt"
         racket/contract
         (only-in "../../../ctcs/common.rkt" class/c*)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt")

(require (only-in racket/function curry))

(require (only-in "message-queue.rkt" enqueue-message!))

(require (only-in racket/dict dict-ref dict-set! dict-has-key?))

(provide cell%
         chars->cell%s
         register-cell-type!
         char->cell%
         empty-cell%
         void-cell%
         wall%
         double-bar?
         door%
         vertical-door%
         other-vertical-door%
         horizontal-door%
         other-horizontal-door%)

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

(define/contract
 cell%
 (configurable-ctc (max (make-cell%/c-with self #\* equal?)) (types cell%/c))
 (class object%
   (inspect #f)
   (init-field (items '()) (occupant #f))
   (define/public (free?) #f)
   (define/public (show) #\*)
   (define/public (open) (enqueue-message! "Can't open that."))
   (define/public (close) (enqueue-message! "Can't close that."))
   (super-new)))

(define/contract
 chars->cell%s
 (configurable-ctc (max (hash/c char? cell%/c)) (types hash?))
 (make-hash))

(define/ctc-helper
 (class-equal? a% b%)
 (and (subclass? a% b%) (subclass? b% a%)))

(define/contract
 (register-cell-type! c% char)
 (configurable-ctc
  (max
   (->i
    ((c% cell%/c) (char char?))
    (result void?)
    #:post
    (c% char)
    (class-equal? (dict-ref chars->cell%s char void) c%)))
  (types (-> cell%/c char? void?)))
 (dict-set! chars->cell%s char c%))

(define/contract
 (char->cell% char)
 (configurable-ctc
  (max
   (->i
    ((char (and/c char? (curry dict-has-key? chars->cell%s))))
    (result
     (char)
     (and/c cell%/c (curry class-equal? (dict-ref chars->cell%s char))))))
  (types (-> char? cell%?)))
 (dict-ref chars->cell%s char))

(register-cell-type! cell% #\*)

(define/contract
 empty-cell%
 (configurable-ctc
  (max
   (make-cell%/c-with
    self
    (or/c #\space (send (get-field occupant self) show))
    (not/c equal?)))
  (types cell%/c))
 (class cell%
   (inspect #f)
   (inherit-field occupant)
   (define/override (free?) (not occupant))
   (define/override
    (show)
    (if occupant (send (or occupant (raise-user-error 'show)) show) #\space))
   (super-new)))

(register-cell-type! empty-cell% #\space)

(define/contract
 void-cell%
 (configurable-ctc (max (make-cell%/c-with self #\. equal?)) (types cell%/c))
 (class cell% (inspect #f) (define/override (show) #\.) (super-new)))

(register-cell-type! void-cell% #\.)

(define/contract
 wall%
 (configurable-ctc (max (make-cell%/c-with self #\X equal?)) (types cell%/c))
 (class cell% (inspect #f) (define/override (show) #\X) (super-new)))

(register-cell-type! wall% #\X)

(define/contract
 double-bar?
 (configurable-ctc (max boolean?) (types boolean?))
 #t)

(define-syntax-rule
 (define-wall name single-bar double-bar)
 (begin
   (define/contract
    name
    (configurable-ctc
     (max
      (make-cell%/c-with self (if double-bar? double-bar single-bar) equal?))
     (types cell%/c))
    (class wall%
      (inspect #f)
      (define/override (show) (if double-bar? double-bar single-bar))
      (super-new)))
   (register-cell-type! name single-bar)
   (register-cell-type! name double-bar)
   (provide name)))

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

(define/contract
 door%
 (configurable-ctc
  (max (make-cell%/c-with self #\* (not/c equal?)))
  (types cell%/c))
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

(define/contract
 vertical-door%
 (configurable-ctc
  (max
   (make-cell%/c-with
    self
    (or/c #\_ (send (get-field occupant self) show))
    (not/c equal?)))
  (types cell%/c))
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

(define/contract
 other-vertical-door%
 (configurable-ctc
  (max
   (make-cell%/c-with
    self
    (or/c #\_ (send (get-field occupant self) show))
    (not/c equal?)))
  (types cell%/c))
 (class vertical-door% (inspect #f) (super-new)))

(register-cell-type! other-vertical-door% #\_)

(define/contract
 horizontal-door%
 (configurable-ctc
  (max
   (make-cell%/c-with
    self
    (or/c #\' (send (get-field occupant self) show))
    (not/c equal?)))
  (types cell%/c))
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

(define/contract
 other-horizontal-door%
 (configurable-ctc
  (max
   (make-cell%/c-with
    self
    (or/c #\' (send (get-field occupant self) show))
    (not/c equal?)))
  (types cell%/c))
 (class horizontal-door% (inspect #f) (super-new)))

(register-cell-type! other-horizontal-door% #\')

