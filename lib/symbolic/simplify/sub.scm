(define (install-sub-package)
  (if (not (get 'traits 'simplify))
    (begin
      (load "lib/symbolic/simplify/traits.scm")
      (install-traits)))

  (put 'simplify '-
    ((get 'trait 'simplify 'debtor) '- '+ - + 0))
  'ok)
