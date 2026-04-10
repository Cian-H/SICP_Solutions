(define (install-symbolic/simplify/sub-package)
  (if (not (get 'traits 'simplify))
    (begin
      (load "lib/symbolic/simplify/traits.scm")
      (install-symbolic/simplify-traits)))

  (put 'simplify '-
    ((get 'trait 'simplify 'debtor) '- '+ - + 0))
  'ok)
