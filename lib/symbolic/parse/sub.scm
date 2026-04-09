(define (install-sub-package)
  (put 'precedence '- 10)
  (put 'associativity '- 'left)
  'ok)
