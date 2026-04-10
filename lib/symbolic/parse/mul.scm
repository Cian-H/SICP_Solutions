(define (install-symbolic/parse/mul-package)
  (put 'precedence '* 20)
  (put 'associativity '* 'right)
  'ok)
