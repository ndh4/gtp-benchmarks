#lang racket

(require racket/mutability
         racket/struct)

(provide my-print
         (struct-out call)
         bug-arg)

(define-values (prop:print-as-λ print-as-λ? get-print-as-λ)
  (make-impersonator-property 'print-as-λ))

(struct call (proc-name proc-kws kw-args pos-args)
  #:prefab)

(define (bug-arg arg)
  (match arg
    [(? procedure?)
     (define io-table (make-hash))
     (chaperone-procedure
      arg
      (make-keyword-procedure
       (λ (kws kw-args . args)
         (values
          (λ results
            (hash-set! io-table
                       (list kws kw-args args)
                       results)
            (apply values results))
          (if (null? kws)
              (apply values args)
              (apply values kw-args args)))))
       prop:print-as-λ
       (λ (port)
         (display "(make-keyword-procedure " port)
         (display "(λ (kws kw-args . args) " port)
         (display "(apply values " port)
         (display "(hash-ref " port)
         (my-print io-table port)
         (display " (list kws kw-args args) '(#f)))))" port)))]
[(? list?)
 (map bug-arg arg)]
[(? pair?)
 (cons (bug-arg car) (bug-arg cdr))]
    [(? mpair?)
     (mcons (bug-arg mcar) (bug-arg mcdr))]
    [(? vector?)
     (define mapped-vec (vector-map bug-arg arg))
     (if (immutable? arg)
         (vector->immutable-vector mapped-vec)
         mapped-vec)]
    [(? generic-set?)
     ((get-list->set arg) (set-map arg bug-arg))]
    [(? hash?)
     ((get-make-hash arg)
      (for/list ([(k v) (in-hash arg)])
        (cons (bug-arg k) (bug-arg v))))]
    [(? struct?)
     (define struct-type (get-first-value (thunk (struct-info arg))))
     (define constructor (struct-type-make-constructor struct-type))
     (apply constructor (map bug-arg (struct->list arg)))]
    [(? box?)
     (define boxer (if (immutable? arg) box-immutable box))
     (boxer (bug-arg (unbox arg)))]

; Cases not explicitly handled:
;    [(or (? symbol?) (? boolean?) (? number?) (? char?))
;     ...]
;    [(? string?)
;     ...]
;    [(? bytes?)
;     ...]
;    [(? object?)
;     ...]
;    [(? void?)
;     ...]
;    [(? class?)
;     ...]
    [else arg]))

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

(define (get-make-hash obj)
  (case (get-hash-label obj)
    [("hash") make-hash]
    [("hashalw") make-hashalw]
    [("hasheqv") make-hasheqv]
    [("hasheq") make-hasheq]
    [("weak-hash") make-weak-hash]
    [("weak-hashalw") make-weak-hashalw]
    [("weak-hasheqv") make-weak-hasheqv]
    [("weak-hasheq") make-weak-hasheq]
    [("ephemeron-hash") make-ephemeron-hash]
    [("ephemeron-hashalw") make-ephemeron-hashalw]
    [("ephemeron-hasheqv") make-ephemeron-hasheqv]
    [("ephemeron-hasheq") make-ephemeron-hasheq]
    [("immutable-hash") make-immutable-hash]
    [("immutable-hashalw") make-immutable-hashalw]
    [("immutable-hasheqv") make-immutable-hasheqv]
    [("immutable-hasheq") make-immutable-hasheq]))

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

(define (get-list->set obj)
  (case (get-set-label obj)
    [("set") list->set]
    [("setalw") list->setalw]
    [("seteqv") list->seteqv]
    [("seteq") list->seteq]
    [("mutable-set") list->mutable-set]
    [("mutable-setalw") list->mutable-setalw]
    [("mutable-seteqv") list->mutable-seteqv]
    [("mutable-seteq") list->mutable-seteq]
    [("weak-set") list->weak-set]
    [("weak-setalw") list->weak-setalw]
    [("weak-seteqv") list->weak-seteqv]
    [("weak-seteq") list->weak-seteq]))

(define (get-first-value generator)
  (call-with-values generator
    (λ args (first args))))

(define (get-struct-name obj)
  (define struct-type (get-first-value (thunk (struct-info obj))))
  (get-first-value (thunk (struct-type-info struct-type))))

(define (get-class-name obj)
  (string-replace (symbol->string (object-name obj)) "object:" ""))

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
    [(? object?)
     (display "(new " port)
     (display (get-class-name obj) port)
     (for ([field-name (field-names obj)])
       (display " (" port)
       (display field-name port)
       (display " " port)
       (my-print (dynamic-get-field field-name obj) port)
       (display ")" port))
     (display ")" port)]
    [(? print-as-λ?) ((get-print-as-λ obj) port)]
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
    [(? class?)
     (display (object-name obj) port)]
    [else
     (fprintf port "#<Unknown: ~a>" obj)]))