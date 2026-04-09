(define (install-mul-package)
  (if (not (get 'traits 'simplify))
    (begin
      (load "lib/symbolic/simplify/traits.scm")
      (install-traits)))

  (define base-mul-simplifier
    ((get 'trait 'simplify 'accumulator) '* * 1))

  (define (simplify-mul lhs rhs)
    (if (or (and (number? lhs) (= lhs 0))
         (and (number? rhs) (= rhs 0)))
      0
      (base-mul-simplifier lhs rhs)))

  (put 'simplify '* simplify-mul)
  'ok)
