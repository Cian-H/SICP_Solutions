(define (install-symbolic/simplify/mul-package)
  (if (not (get 'traits 'simplify))
    (begin
      (load "lib/symbolic/simplify/traits.scm")
      (install-symbolic/simplify-traits)))

  (define base-mul-simplifier
    ((get 'trait 'simplify 'accumulator) '* '/ * 1))

  (define (simplify-mul lhs rhs)
    (cond
      ((or (and (number? lhs) (= lhs 0)) (and (number? rhs) (= rhs 0))) 0)
      ;; Fractional cancellation rule: c * (expr / c) -> expr
      ((and (number? lhs) (pair? rhs) (eq? (type-of rhs) '/)
          (number? (cadr (type-unwrap rhs)))
          (= lhs (cadr (type-unwrap rhs))))
        (car (type-unwrap rhs)))
      (else (base-mul-simplifier lhs rhs))))

  (put 'simplify '* simplify-mul)
  'ok)
