(define (install-symbolic/calculus/ln-package)
  (define (ln-val operands) (car operands))

  (define (make-ln x)
    (cond
      ((and (number? x) (= x 1)) 0)
      ((and (pair? x) (eq? (type-of x) 'constant) (eq? (car (type-unwrap x)) 'e)) 1)
      (else (type-wrap 'ln (list x)))))

  (define (deriv-ln operands var)
    (let ((u (ln-val operands)))
      (div (deriv u var) u)))

  (put 'deriv 'ln deriv-ln)
  (put 'make 'ln make-ln)
  'ok)
