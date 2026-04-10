(define (install-symbolic/parse/prefix-package)
  (define (parse-prefix expr)
    (cond
      ((not (pair? expr))
        (if (symbol? expr)
          (let ((constant-pred (get 'predicate 'constant)))
            (if (and constant-pred (constant-pred expr))
              ((get 'make 'constant) expr)
              expr))
          expr))
      (else
        (let* ((op (car expr))
               (operands (cdr expr))
               (constructor (get 'make op))
               (parsed-operands (map parse-prefix operands)))
          (if constructor
            (apply constructor parsed-operands)
            (type-wrap op parsed-operands))))))

  (define (unparse-prefix expr)
    (cond
      ((not (pair? expr)) expr)
      ((eq? (type-of expr) 'constant) (car (type-unwrap expr)))
      (else
        (cons (type-of expr) (map unparse-prefix (type-unwrap expr))))))

  (put 'parser 'prefix parse-prefix)
  (put 'unparser 'prefix unparse-prefix)
  'ok)
