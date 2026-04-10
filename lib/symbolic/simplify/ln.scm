(define (install-symbolic/simplify/ln-package)
  (define (simplify-ln operand)
    (cond
      ((number? operand) (log operand))
      ;; Property: ln(e) -> 1
      ((and (pair? operand) (eq? (type-of operand) 'constant) (eq? (car (type-unwrap operand)) 'e)) 1)
      ;; Property: ln(e^x) -> x
      ((and (pair? operand) (eq? (type-of operand) '**))
        (let ((base (car (type-unwrap operand)))
              (exp (cadr (type-unwrap operand))))
          (if (and (pair? base) (eq? (type-of base) 'constant) (eq? (car (type-unwrap base)) 'e))
            exp
            (type-wrap 'ln (list operand)))))

      (else (type-wrap 'ln (list operand)))))

  (put 'simplify 'ln simplify-ln)
  'ok)
