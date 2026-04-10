(define (install-symbolic/algebra/log-package)
  (define (isolate-log operands rhs var)
    (let ((base (car operands))
          (val (cadr operands))
          (smart-exp (make-smart-constructor '** 'exp)))
      (cond
        ((contains-var? val var)
          ;; log_base(val) = rhs -> val = base^rhs
          (solve (list '= val (smart-exp base rhs)) var))
        ((contains-var? base var)
          ;; log_base(val) = rhs -> base^rhs = val -> base = val^(1/rhs)
          (solve (list '= base (smart-exp val (div 1 rhs))) var))
        (else (error "Variable not found in log expression")))))

  (put 'isolate '// isolate-log)
  'ok)
