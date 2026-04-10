(define (install-symbolic/parse/infix/sub-package)
  (put 'precedence '- 10)
  (put 'associativity '- 'left)
  'ok)
