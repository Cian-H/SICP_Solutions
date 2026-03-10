(define (type-wrap s x)
  (cons s x))

(define (type-of x)
  (cond
    ((not (pair? x)) (error "procedure `type-of` only accepts pairs!"))
    ((symbol? (car x)) (car x))
    (else (error "procedure `type-of` expects `car` to return a symbol!"))))

(define (type-unwrap x) (cdr x))
