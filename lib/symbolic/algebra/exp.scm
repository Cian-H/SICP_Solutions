(define (install-symbolic/algebra/exp-package)
  (define (isolate-exp operands rhs var)
    (let* ((base (car operands))
           (exponent (cadr operands))
           (e-const ((get 'make 'constant) 'e)))

      (cond
        ((contains-var? base var)
          ;; Variable is in the base: base = rhs^(1/exponent)
          (let ((smart-exp (make-smart-constructor '** 'exp)))
            (solve (list '= base (smart-exp rhs (div 1 exponent))) var)))
        ((contains-var? exponent var)
          ;; Variable is in the exponent: exponent = log_base(rhs)
          (if (equal? base e-const)
            ;; Special Case: If the base is 'e', strictly use the 'ln' constructor
            (solve (list '= exponent (ln rhs)) var)
            ;; General Case: Use custom base log ('//').
            (let ((smart-log (make-smart-constructor '// 'log)))
              (solve (list '= exponent (smart-log base rhs)) var))))

        (else (error "Variable not found in exp expression")))))

  (put 'isolate '** isolate-exp)
  'ok)
