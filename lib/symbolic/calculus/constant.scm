(define (install-symbolic/calculus/constant-package)
  (put 'deriv 'constant (lambda (operands var) 0))
  'ok)
