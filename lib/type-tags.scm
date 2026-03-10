(define (attach-tag s x)
  (cons s x))

(define (type-tag x)
  (cond
    ((not (pair? x)) (error "procedure `type-tag` only accepts pairs!"))
    ((symbol? (car x)) (car x))
    (else (error "procedure `type-tag` expects `car` to return a symbol!"))))

(define (contents x) (cdr x))
