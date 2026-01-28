(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier s) (cadr s))
(define (multiplicand s) (caddr s))

(define (=number? expr num) (and (number? expr) (= expr num)))

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2))))

; Just for fun, i was curious if i could define a procedure that turns these symbols into a lambda.
; Turns out: you can. This is a very cool little capability to add!
(load "lib/functools.scm")
(define algebraic-ops (list '** '* '/ '+ '-)) ; Ordered according to BEMDAS

(define (algebraic-op? x)
  (if (memq x algebraic-ops) #t #f))

(define (filter-vars l)
  (unique (filter
           (lambda (x) (and (variable? x) (not (algebraic-op? x))))
           (enumerate-tree l))))

(define (expr->lambda expr)
  (eval (list 'lambda (filter-vars expr) expr)
    (interaction-environment)))

; ~~~ Exercise start ~~~
(define (make-exponentiation b e)
  (cond
    ((=number? e 0) 1)
    ((=number? b 0) 0)
    ((=number? e 1) b)
    ((and (number? b) (number? e)) (expt b e))
    (else (list '** b e))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (deriv expr var)
  (cond ((number? expr) 0)
    ((variable? expr) (if (same-variable? expr var) 1 0))
    ((sum? expr) (make-sum (deriv (addend expr) var)
                  (deriv (augend expr) var)))
    ((product? expr) (make-sum
                      (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
                      (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
    ((exponentiation? expr) (make-product
                             (make-product
                               (exponent expr)
                               (make-exponentiation
                                 (base expr)
                                 (make-sum (exponent expr) -1)))
                             (deriv (base expr) var)))
    (else (error "Unknown expression type: DERIV"))))
