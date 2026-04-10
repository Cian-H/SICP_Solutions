(define (parse expr)
  (cond
    ((null? expr) expr)
    ((number? expr) expr)
    ((symbol? expr)
      (let ((constant-pred (get 'predicate 'constant)))
        (if (and constant-pred (constant-pred expr))
          ((get 'make 'constant) expr)
          expr)))
    ((null? (cdr expr)) (parse (car expr)))
    ((pair? expr)
      (let* ((op (lowest-precedence-op expr))
             (constructor (get 'make op))
             (lhs (parse (get-prefix op expr)))
             (rhs (parse (get-suffix op expr))))
        (if constructor
          (constructor lhs rhs)
          (type-wrap op (list lhs rhs)))))
    (else expr)))

(define (lowest-precedence-op expr)
  (let* ((ops (filter (lambda (x) (and (symbol? x) (get 'precedence x))) expr))
         (precs (map (lambda (x) (get 'precedence x)) ops))
         (min-prec (apply min precs)))
    ;; Find all operators that share this lowest precedence level
    (let* ((min-ops (filter (lambda (op) (= (get 'precedence op) min-prec)) ops))
           (sample-op (car min-ops))
           (associativity (get 'associativity sample-op)))
      ;; Resolve ties based on associativity
      (if (eq? associativity 'right)
        (car min-ops) ; First occurrence for right-associative
        (car (reverse min-ops)))))) ; Last occurrence for left-associative

(define (get-prefix op expr)
  (let ((associativity (get 'associativity op)))
    (cond
      ((eq? associativity 'right) (take-while (lambda (x) (not (eq? x op))) expr))
      ((eq? associativity 'left) (reverse (cdr (memq op (reverse expr)))))
      (else (error "Unknown associativity for prefix" op)))))

(define (get-suffix op expr)
  (let ((associativity (get 'associativity op)))
    (cond
      ((eq? associativity 'right) (cdr (memq op expr)))
      ((eq? associativity 'left) (reverse (take-while (lambda (x) (not (eq? x op))) (reverse expr))))
      (else (error "Unknown associativity for suffix" op)))))

(define (unparse expr)
  (cond
    ((not (pair? expr)) expr)
    ((eq? (type-of expr) 'constant) (car (type-unwrap expr)))
    ((= (length expr) 3)
      (let ((op (car expr))
            (lhs (cadr expr))
            (rhs (caddr expr)))
        (list (unparse lhs) op (unparse rhs))))
    ((= (length expr) 2)
      (let ((op (car expr))
            (val (cadr expr)))
        (list op (unparse val))))
    (else (map unparse expr))))
