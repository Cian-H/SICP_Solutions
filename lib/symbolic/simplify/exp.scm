(define (install-exp-package)
  (define (simplify-exp lhs rhs)
    (cond
      ((and (number? lhs) (number? rhs)) (expt lhs rhs))
      ((and (number? rhs) (= rhs 0)) 1)
      ((and (number? rhs) (= rhs 1)) lhs)
      ((and (number? lhs) (= lhs 0)) 0)
      ((and (number? lhs) (= lhs 1)) 1)
      (else (type-wrap '** (list lhs rhs)))))

  (put 'simplify '** simplify-exp)
  'ok)
