#lang racket

(require racket/contract
         (only-in racket/list first empty? rest)
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/configurable.rkt")

(provide/configurable-contract
 [message-queue ([max (listof string?)]
   [types (listof string?)])]
 [enqueue-message! ([max (let ([pre/queue-len #f]
              [pre/queue message-queue])
          (->i ([m string?])
               #:pre () (begin (set! pre/queue-len (length message-queue))
                               (set! pre/queue-len message-queue))
               [result void?]
               #:post (m) (and (equal? pre/queue (rest message-queue))
                               (= (length message-queue)
                                  (add1 pre/queue-len))
                               (string=? m (first message-queue)))))]
   [types (string? . -> . void?)])]
 [reset-message-queue! ([max (->* () ()
             void?
             #:post (empty? message-queue))]
   [types (-> void?)])])

;; (provide
;;   enqueue-message!
;;   reset-message-queue!
;; )


;; list of strings (messages) which were produced since the previous
;; previous display, and need to be displayed now
(define message-queue
  '())

(define (enqueue-message! m)
  (set! message-queue (cons m message-queue)))

(define (reset-message-queue!)
  (set! message-queue '()))
