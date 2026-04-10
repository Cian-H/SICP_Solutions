(define (deriv expr var)
  (cond ((number? expr) 0)
    ((symbol? expr)
      (let ((constant? (get 'predicate 'constant)))
        (if (and constant? (constant? expr))
          0
          (if (eq? expr var) 1 0))))
    (else
      (let* ((expr-type (type-of expr))
             (deriv-proc (get 'deriv expr-type)))
        (if deriv-proc
          (deriv-proc (type-unwrap expr) var)
          (error "Unknown expression type -- DERIV" expr-type))))))
