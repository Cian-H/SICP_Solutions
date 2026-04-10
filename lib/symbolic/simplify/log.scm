(define (install-symbolic/simplify/log-package)
  (define (simplify-log lhs rhs)
    (cond
      ((and (number? rhs) (= rhs 1)) 0)
      ((equal? lhs rhs) 1)
      ;; Property: log(b, x^y) -> y * log(b, x)
      ((and (pair? rhs) (eq? (type-of rhs) '**))
        (let ((base-inner (car (type-unwrap rhs)))
              (exponent (cadr (type-unwrap rhs))))
          (if (equal? lhs base-inner)
            exponent ; log(b, b^x) -> x
            (let ((mul-simp (get 'simplify '*)))
              (if mul-simp
                (mul-simp exponent (simplify-log lhs base-inner))
                (type-wrap '* (list exponent (simplify-log lhs base-inner))))))))

      (else (type-wrap '// (list lhs rhs)))))
  (put 'simplify '// simplify-log)
  'ok)
