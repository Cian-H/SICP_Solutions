(define (install-log-package)
  (define (simplify-log lhs rhs)
    (cond
      ((and (number? lhs) (number? rhs)) (/ (log rhs) (log lhs)))
      ((and (number? rhs) (= rhs 1)) 0)
      ((equal? lhs rhs) 1)

      ;; Property: log(b, b^x) -> x
      ((and (pair? rhs) (eq? (type-of rhs) '**) (equal? lhs (car (type-unwrap rhs))))
        (cadr (type-unwrap rhs)))

      (else (type-wrap '// (list lhs rhs)))))

  (put 'simplify '// simplify-log)
  'ok)
