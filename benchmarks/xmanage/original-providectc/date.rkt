#lang racket

;; a simple data representation of dates

(provide #; Date today my-date? next-month)

#; {type Date = [List Natural Natural Natural]}

(define (my-date? x)
  (match x
    [(list (? natural? year) (? natural? month) (? natural? day))
     #true]
    [_
     (error 'my-date "date expected, found ~a" x)]))
  
#; { -> Date}
(define (today)
  (let ([d (seconds->date (current-seconds))])
    (list (date-year d) (date-month d) (date-day d))))

#; {Date Date -> Date}
(define (next-month date1 date2)
  (match-define [list _year1 month1 _day1] date1)
  (match-define [list _year2 month2 _day2] date2)
  (or (< month1 month2)
      (and (= month1 12)
           (< month2 12))))

(module+ test
  (require rackunit)

  (check-true (my-date? (today)))
  (check-exn #px"date expected" (λ () (my-date? '(1 2 3 4)))))