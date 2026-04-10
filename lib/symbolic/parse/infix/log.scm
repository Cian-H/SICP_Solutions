(define (install-symbolic/parse/infix/log-package)
  (put 'precedence '// 30)
  (put 'associativity '// 'left)
  'ok)
