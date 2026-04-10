(define (install-symbolic/parse/infix/eq-package)
  (put 'precedence '= 0)
  (put 'associativity '= 'right)
  'ok)
