(define (install-symbolic/parse/postfix-package)
  (define (parse-postfix expr)
    (cond
      ((not (pair? expr))
        (if (symbol? expr)
          (let ((constant-pred (get 'predicate 'constant)))
            (if (and constant-pred (constant-pred expr))
              ((get 'make 'constant) expr)
              expr))
          expr))
      (else
        (let* ((op (last expr))
               (operands (take expr (- (length expr) 1)))
               (constructor (get 'make op))
               (parsed-operands (map parse-postfix operands)))
          (if constructor
            (apply constructor parsed-operands)
            (type-wrap op parsed-operands))))))

  (define (unparse-postfix expr)
    (cond
      ((not (pair? expr)) expr)
      ((eq? (type-of expr) 'constant) (car (type-unwrap expr)))
      (else
        (append (map unparse-postfix (type-unwrap expr))
          (list (type-of expr))))))

  (put 'parser 'postfix parse-postfix)
  (put 'unparser 'postfix unparse-postfix)
  'ok)
