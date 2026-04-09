(define (install-log-package)
  (if (not (get 'deriv 'ln))
    (safe-load "lib/symbolic/calculus/ln.scm"))
  (install-ln-package)

  (define (log-base operands) (car operands))
  (define (log-val operands) (cadr operands))

  (define (make-log b val)
    (cond ((and (number? b) (number? val)) (/ (log val) (log b)))
      ((eq? val 1) 0)
      ((equal? b val) 1)
      (else (type-wrap '// (list b val)))))

  (define (deriv-log operands var)
    (let ((u (log-base operands))
          (v (log-val operands)))
      (if (or (number? u) (eq? (type-of u) 'constant))
        ;; If base is constant: d(log_u(v))/dx = (dv/dx) / (v * ln(u))
        (div (deriv v var)
          (mul v (ln u)))
        ;; If base is variable: fallback to change-of-base formula
        ;; and let the generic div/deriv packages handle the quotient rule
        (deriv (div (ln v) (ln u)) var))))

  (put 'deriv '// deriv-log)
  (put 'log '// make-log)
  'ok)
