#lang racket

(require racket/contract
         (only-in racket/list first empty? rest)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/configurable.rkt")

(provide/configurable-contract
 [message-queue ([max (box/c (listof string?))]
   [types (box/c (listof string?))])]
 [enqueue-message! ([max (let ([pre/queue-len #f]
              [pre/queue message-queue])
          (->i ([m string?])
               #:pre () (begin (set! pre/queue-len (length (unbox message-queue)))
                               (set! pre/queue (unbox message-queue)))
               [result void?]
               #:post (m) (and (equal? pre/queue (rest (unbox message-queue)))
                               (= (length (unbox message-queue))
                                  (add1 pre/queue-len))
                               (string=? m (first (unbox message-queue))))))]
   [types (string? . -> . void?)])]
 [reset-message-queue! ([max (->* () ()
             void?
             #:post (empty? (unbox message-queue)))]
   [types (-> void?)])])

;; (provide
;;   enqueue-message!
;;   reset-message-queue!
;; )


;; list of strings (messages) which were produced since the previous
;; previous display, and need to be displayed now
(define message-queue
  (box '()))

(define (enqueue-message! m)
  (set-box! message-queue (cons m (unbox message-queue))))

(define (reset-message-queue!)
  (set-box! message-queue '()))

(module+ test
  (require rackunit)
  (check-equal? (unbox message-queue) '())
  (enqueue-message! "A very important message")
  (enqueue-message! "Another very important message")
  (check-equal? (unbox message-queue)
                (list "Another very important message"
                      "A very important message"))
  (reset-message-queue!)
  (check-equal? (unbox message-queue) '()))
