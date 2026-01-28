#lang racket


(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         "data.rkt")

(provide/configurable-contract)

(provide substring?
         ordered-substrings?
         count-strs-in-lst
         sublist?
         check-station-pairs?
         lines-in-color-file?
         valid-pair?)


(define/ctc-helper (substring? maybe-substr str)
  (string-contains? str maybe-substr)
  #;(cond
    [(< (string-length str) (string-length maybe-substr)) #f]
    [(string=? (substring str 0 (string-length maybe-substr)) maybe-substr)
     #t]
    [else (substring? maybe-substr (substring str 1))]))

(define/ctc-helper (ordered-substrings? lst str)
  (cond
    [(empty? lst) #t]
    [else (and (substring? (first lst) str)
               (ordered-substrings? (rest lst) str))]))

(define/ctc-helper (count-strs-in-lst lst)
  (count string? lst)
  #;(cond
    [(empty? lst) 0]
    [else (if (string? (first lst))
              (+ 1 (count-strs-in-lst (rest lst)))
              (count-strs-in-lst (rest lst)))]))

(define/ctc-helper (sublist? maybe-sublst lst)
  (cond
    #;[(and (empty? maybe-sublst) (empty? lst) #t)]
    [(empty? maybe-sublst) #t]
    [(empty? lst) #f]
    [else (define str (string-trim (first lst)))
          (if (string=? str (first maybe-sublst))
              (sublist? (rest maybe-sublst) (rest lst))
              (sublist? maybe-sublst (rest lst)))]))

;;-----------------------------------------------------------------------------
(define/ctc-helper (check-station-pairs? station-pairs)
  (cond
    [(empty? station-pairs) #t]
    [else (let ([pair1 (first station-pairs)]
                [pair2 (second station-pairs)])
            (if (and (valid-pair? pair1)
                     (valid-pair? pair2)
                     (equivalent-pairs? pair1 pair2))
                (check-station-pairs? (rest (rest station-pairs)))
                #f))]))

(define/ctc-helper (equivalent-pairs? pair1 pair2)
  (and (string=? (first pair1) (second pair2))
       (string=? (second pair1) (first pair2))))

(define/ctc-helper (valid-pair? pair)
  (and (= (length pair) 2)
       (station? (first pair))
       (station? (second pair))
       (not (string=? (first pair)
                      (second pair)))))

;;-----------------------------------------------------------------------------
(define/ctc-helper (lines-in-color-file? lines color-file)
  (cond
    #;[(and (empty? lines) (empty? color-file)) #t]
    [(empty? lines) #t]
    [(empty? color-file) #f]
    [else (if (substring? (first lines) (first color-file))
              (lines-in-color-file? (rest lines) (rest color-file))
              (lines-in-color-file? lines (rest color-file)))]))
