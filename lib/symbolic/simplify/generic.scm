(define (simplify expr)
  (if (not (pair? expr))
    expr
    (let* ((type (type-of expr))
           (operands (type-unwrap expr))
           (simplified-operands (map simplify operands))
           (simplify-proc (get 'simplify type)))
      (if simplify-proc
        (apply simplify-proc simplified-operands)
        (type-wrap type simplified-operands)))))
