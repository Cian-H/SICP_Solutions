(define (install-symbolic/algebra/ln-package)
  (define (isolate-ln operands rhs var)
    (let ((val (car operands))
          ;; Safely fetch the exact AST node for the constant 'e'
          (e-const ((get 'make 'constant) 'e)))
      (if (contains-var? val var)
        ;; ln(val) = rhs -> val = e^rhs
        (let ((smart-exp (make-smart-constructor '** 'exp)))
          (solve (list '= val (smart-exp e-const rhs)) var))
        (error "Variable not found in ln expression"))))

  (put 'isolate 'ln isolate-ln)
  'ok)
