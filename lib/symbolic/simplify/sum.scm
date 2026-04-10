(define (install-symbolic/simplify/sum-package)
  (if (not (get 'traits 'simplify))
    (begin
      (load "lib/symbolic/simplify/traits.scm")
      (install-symbolic/simplify-traits)))

  (put 'simplify '+
    ((get 'trait 'simplify 'accumulator) '+ '- + 0))
  'ok)
