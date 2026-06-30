#lang racket

(require racket/mutability
         racket/struct)

(provide my-print)

(define (my-construct-print name elem-stream port)
  (display "(" port)
  (display name port)
  (for ([elem elem-stream])
    (display " " port)
    (my-print elem port))
  (display ")" port))

(define (my-string-print obj name port)
  (cond
    [(immutable? obj)
     (print obj port)]
    [else
     (display "(" port)
     (display name port)
     (display "-append " port)
     (print obj port)
     (display ")" port)]))

(define (get-hash-label obj)
  (define strength
    (cond
      [(hash-strong? obj)
       (cond
         [(immutable? obj) "immutable-"]
         [else ""])]
      [(hash-weak? obj) "weak-"] ;; also implies mutable
      [(hash-ephemeron? obj) "ephemeron-"] ;; also implies mutable
      [else "unknownstrength-"]))

  (define comparison-type
    (cond
      [(hash-equal? obj) ""]
      [(hash-equal-always? obj) "alw"]
      [(hash-eqv? obj) "eqv"]
      [(hash-eq? obj) "eq"]))

  (format "~ahash~a" strength comparison-type))

(define (get-set-label obj)
  (define strength
    (cond
      [(set? obj) ""] ;; immutable
      [(set-mutable? obj) "mutable-"] ;; mutable with strongly-held keys
      [(set-weak? obj) "weak-"] ;; mutable with weakly-held keys
      [else "unknownstrength-"]))

  (define comparison-type
    (cond
      [(set-equal? obj) ""]
      [(set-equal-always? obj) "alw"]
      [(set-eqv? obj) "eqv"]
      [(set-eq? obj) "eq"]))

  (format "~aset~a" strength comparison-type))

(define (get-struct-name obj)
  (define-values
    (struct-type skipped?)
    (struct-info obj))
  (define-values
    (name init-field-cnt auto-field-cnt accessor-proc mutator-proc immutable-k-list super-type skipped-again?)
    (struct-type-info struct-type))
  name)

(define (my-print obj port)
  (match obj
    [(or (? symbol?) (? boolean?) (? number?) (? char?))
     (print obj port)]
    [(? string?)
     (my-string-print obj 'string port)]
    [(? bytes?)
     (my-string-print obj 'bytes port)]
    [(? list?)
     (my-construct-print 'list (in-list obj) port)]
    [(? pair?)
     (my-construct-print 'cons (stream (car obj) (cdr obj)) port)]
    [(? mpair?)
     (my-construct-print 'mcons (stream (mcar obj) (mcdr obj)) port)]
    [(? vector?)
     (define label (if (immutable? obj) 'vector-immutable 'vector))
     (my-construct-print label (in-vector obj) port)]
    [(? hash?)
     (display "(make-" port)
     (display (get-hash-label obj) port)
     (display " (list" port)
     (for ([(k v) (in-hash obj)])
       (display " (cons " port)
       (my-print k port)
       (display " " port)
       (my-print v port)
       (display ")" port))
     (display "))" port)]
    [(? generic-set?)
     (my-construct-print (get-set-label obj) (in-set obj) port)]
    [(? struct?)
     (define name (get-struct-name obj))
     (my-construct-print name (in-list (struct->list obj)) port)]
    [(? box?)
     (define label (if (immutable? obj) 'box-immutable 'box))
     (my-construct-print label (stream (unbox obj)) port)]
    [(? void?)
     (my-construct-print 'void empty-stream port)]
    [(? procedure?)
     (my-construct-print 'some-procedure empty-stream port)]
    [else
     (fprintf port "#<Unknown: ~a>" obj)]))