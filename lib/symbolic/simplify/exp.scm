;; symbolic/simplify/exp.scm

(define (install-symbolic/simplify/exp-package)
  (define (simplify-exp lhs rhs)
    (cond
      ((and (number? lhs) (number? rhs)) (expt lhs rhs))
      ((and (number? rhs) (= rhs 0)) 1)
      ((and (number? rhs) (= rhs 1)) lhs)
      ((and (number? lhs) (= lhs 0)) 0)
      ((and (number? lhs) (= lhs 1)) 1)
      ;; Rule: Power of a Power (a^b)^c -> a^(b*c)
      ((and (pair? lhs) (eq? (type-of lhs) '**))
        (let ((a (car (type-unwrap lhs)))
              (b (cadr (type-unwrap lhs)))
              (smart-exp (make-smart-constructor '** 'exp)))
          (smart-exp a (mul b rhs))))

      (else (type-wrap '** (list lhs rhs)))))

  (put 'simplify '** simplify-exp)
  'ok)
