#lang racket

(require "../../../ctcs/configurable.rkt" "../../../ctcs/precision-config.rkt")

(provide today my-date? next-month?)

(provide my-date/c current-seconds/maybe-fake)

(define/ctc-helper current-seconds/maybe-fake (make-parameter current-seconds))

(define/ctc-helper
 my-date/c
 (list/c natural-number/c natural-number/c natural-number/c))

(define/contract
 (my-date? x)
 (configurable-ctc (max (-> my-date/c #t)) (types (-> any/c boolean?)))
 (match
  x
  ((list (? natural? year) (? natural? month) (? natural? day)) #t)
  (_ (error 'my-date "date expected, found ~a" x))))

(define/contract
 (today)
 (configurable-ctc (max (-> my-date/c)) (types (-> my-date/c)))
 (let ((d (seconds->date ((current-seconds/maybe-fake)))))
   (list (date-year d) (date-month d) (date-day d))))

(define/contract
 (next-month? date1 date2)
 (configurable-ctc
  (max (-> my-date/c my-date/c boolean?))
  (types (-> my-date/c my-date/c boolean?)))
 (match-define (list _year1 month1 _day1) date1)
 (match-define (list _year2 month2 _day2) date2)
 (or (< month1 month2) (and (= month1 12) (< month2 12))))

(module+
 test
 (require rackunit)
 (current-seconds/maybe-fake (thunk 1784630918))
 (check-true (my-date? (today))))

