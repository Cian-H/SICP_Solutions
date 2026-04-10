(define (install-symbolic/compile-package)
  (put 'native-op '+ +)
  (put 'native-op '- -)
  (put 'native-op '* *)
  (put 'native-op '/ /)
  (put 'native-op '** expt)
  (put 'native-op 'ln log)
  (put 'native-op '// (lambda (b x) (/ (log x) (log b))))
  'ok)

(define (compile-expr expr vars)
  (let ((optimized-expr (simplify (evaluate-constants expr))))
    (define (build-evaluator tree)
      (cond
        ;; Numbers return themselves
        ((number? tree)
          (lambda args tree))
        ;; If a constant somehow survived evaluation, return its value
        ((and (pair? tree) (eq? (type-of tree) 'constant))
          (let ((val (cadr (type-unwrap tree))))
            (lambda args val)))
        ;; Variables fetch their value from the arguments list by index
        ((symbol? tree)
          (let ((idx (list-index (lambda (v) (eq? v tree)) vars)))
            (if idx
              (lambda args (list-ref args idx))
              (error "Variable not found in compilation arguments list:" tree))))
        ;; Compound expressions apply the native operation to evaluated children
        ((pair? tree)
          (let* ((type (type-of tree))
                 (operands (type-unwrap tree))
                 (native-fn (get 'native-op type))
                 (evaluators (map build-evaluator operands)))
            (if native-fn
              (lambda args
                (apply native-fn (map (lambda (ev) (apply ev args)) evaluators)))
              (error "No native procedure registered for type:" type))))
        (else (error "Unknown AST node type:" tree))))
    ;; Return the final, callable closure
    (build-evaluator optimized-expr)))
