(define (install-symbolic/simplify/div-package)
  (if (not (get 'traits 'simplify))
    (begin
      (load "lib/symbolic/simplify/traits.scm")
      (install-symbolic/simplify-traits)))

  (define base-div-simplifier
    ((get 'trait 'simplify 'debtor) '/ '* / * 1))

  (define (simplify-div lhs rhs)
    (if (and (number? lhs) (= lhs 0))
      0
      (base-div-simplifier lhs rhs)))

  (put 'simplify '/ simplify-div)
  'ok)
