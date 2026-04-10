(define (install-symbolic/algebra/sub-package)
  (define (isolate-sub operands rhs var)
    (let ((a (car operands))
          (b (cadr operands)))
      (cond
        ((contains-var? a var)
          (solve (list '= a (add rhs b)) var))
        ((contains-var? b var)
          (solve (list '= b (sub a rhs)) var))

        (else (error "Variable not found in subtraction expression")))))

  (put 'isolate '- isolate-sub)
  'ok)
