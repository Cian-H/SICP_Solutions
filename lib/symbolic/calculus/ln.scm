(define (install-symbolic/calculus/ln-package)
  (define (ln-val operands) (car operands))

  (define (make-ln x)
    (cond
      ((number? x) (log x))
      ((and (pair? x) (eq? (type-of x) 'constant) (eq? (car (type-unwrap x)) 'e)) 1)
      (else (type-wrap 'ln (list x)))))

  (define (deriv-ln operands var)
    (let ((u (ln-val operands)))
      ;; d(ln u) / dx = (du / dx) / u
      (div (deriv u var) u)))

  (put 'deriv 'ln deriv-ln)
  (put 'make 'ln make-ln)
  'ok)
