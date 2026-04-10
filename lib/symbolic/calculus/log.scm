(define (install-symbolic/calculus/log-package)
  (if (not (get 'deriv 'ln))
    (safe-load "lib/symbolic/calculus/ln.scm"))
  (install-symbolic/calculus/ln-package)

  (define (log-base operands) (car operands))
  (define (log-val operands) (cadr operands))

  (define (make-log b val)
    (cond ((and (number? val) (= val 1)) 0)
      ((equal? b val) 1)
      (else (type-wrap '// (list b val)))))

  (define (deriv-log operands var)
    (let ((u (log-base operands))
          (v (log-val operands)))
      (if (or (number? u) (eq? (type-of u) 'constant))
        (div (deriv v var)
          (mul v (ln u)))
        (deriv (div (ln v) (ln u)) var))))

  (put 'deriv '// deriv-log)
  (put 'log '// make-log)
  'ok)
