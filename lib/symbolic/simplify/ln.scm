(define (install-ln-package)
  (define (simplify-ln operand)
    (cond
      ((number? operand) (log operand))
      ((eq? operand 'e) 1)

      ;; Property: ln(e^x) -> x
      ((and (pair? operand) (eq? (type-of operand) '**) (eq? (car (type-unwrap operand)) 'e))
        (cadr (type-unwrap operand)))

      (else (type-wrap 'ln (list operand)))))

  (put 'simplify 'ln simplify-ln)
  'ok)
