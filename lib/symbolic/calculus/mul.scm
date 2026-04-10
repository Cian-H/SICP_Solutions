(define (install-symbolic/calculus/mul-package)
  (define (multiplier operands) (car operands))
  (define (multiplicand operands) (cadr operands))

  (define (make-mul x y)
    (cond ((and (number? x) (number? y)) (* x y))
      ((or (eq? x 0) (eq? y 0)) 0)
      ((eq? x 1) y)
      ((eq? y 1) x)
      (else (type-wrap '* (list x y)))))

  (define (deriv-mul operands var)
    (let ((u (multiplier operands))
          (v (multiplicand operands)))
      (add (mul u (deriv v var))
        (mul v (deriv u var)))))

  (define (integrate-mul operands var)
    (let ((u (multiplier operands))
          (v (multiplicand operands)))
      ;; Helper to check if an expression contains the integration variable
      (define (free-of? expr)
        (cond
          ((number? expr) #t)
          ((symbol? expr) (not (eq? expr var)))
          ((and (pair? expr) (eq? (type-of expr) 'constant)) #t)
          ((pair? expr)
            ;; Recursively check all operands in the compound expression
            (let loop ((ops (type-unwrap expr)))
              (cond ((null? ops) #t)
                ((not (free-of? (car ops))) #f)
                (else (loop (cdr ops))))))
          (else #t)))

      (cond
        ((free-of? u) (mul u (integrate v var)))
        ((free-of? v) (mul v (integrate u var)))
        (else (list 'integral (type-wrap '* operands) 'd var)))))

  (put 'deriv '* deriv-mul)
  (put 'integrate '* integrate-mul)
  (put 'mul '* make-mul)
  'ok)
