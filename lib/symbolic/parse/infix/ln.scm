(define (install-symbolic/parse/infix/ln-package)
  (put 'precedence 'ln (or (get 'precedence '//) 30))
  (put 'associativity 'ln 'unary)
  'ok)
