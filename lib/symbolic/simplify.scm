;;; Expression Simplifier
; First, we define associativity rules
(define right-associative-ops '(+ * **))
(define left-associative-ops '(- / //))

(define (right-associative? s) (if (memq s right-associative-ops) #t #f))
(define (left-associative? s) (if (memq s left-associative-ops) #t #f))

(define (get-prefix s expr)
  (let ((res (cond
              ((right-associative? s) (take-while (lambda (x) (not (eq? x s))) expr))
              ((left-associative? s) (reverse (cdr (memq s (reverse expr)))))
              (else (error "Unknown associativity" s)))))
    (cond
      ((null? res) (error "Empty prefix for operator" s))
      ((null? (cdr res)) (car res))
      (else res))))

(define (get-suffix s expr)
  (let ((res (cond
              ((right-associative? s) (cdr (memq s expr)))
              ((left-associative? s) (reverse (take-while (lambda (x) (not (eq? x s))) (reverse expr))))
              (else (error "Unknown associativity" s)))))
    (cond
      ((null? res) (error "Empty suffix for operator" s))
      ((null? (cdr res)) (car res))
      (else res))))
; Then, we define simplification rules for specific subtypes of operators
(define (simplify-unique-op a b maker-std _maker-inv _op-std _op-inv _predicate _left-sel _right-sel)
  (maker-std a b))

(define (simplify-accumulator-op a b maker-std _maker-inv op-std _op-inv predicate left-sel right-sel)
  (cond
    [(and (number? a) (pair? b) (predicate b) (number? (left-sel b))) (maker-std (op-std a (left-sel b)) (right-sel b))]
    [(and (number? b) (pair? a) (predicate a) (number? (right-sel a))) (maker-std (left-sel a) (op-std b (right-sel a)))]
    [else (maker-std a b)]))

(define (simplify-debtor-op a b maker-std maker-inv op-std op-inv predicate left-sel right-sel)
  (cond
    [(and (number? a) (pair? b) (predicate b) (number? (left-sel b))) (maker-inv (op-std a (left-sel b)) (right-sel b))]
    [(and (number? b) (pair? a) (predicate a) (number? (right-sel a))) (maker-std (left-sel a) (op-inv b (right-sel a)))]
    [else (maker-std a b)]))

(define simplification-rules
  (list
    (list '+ simplify-accumulator-op make-sum inverse-sum + - sum? addend augend)
    (list '- simplify-debtor-op make-sub inverse-sub - + sub? minuend subtrahend)
    (list '* simplify-accumulator-op make-product inverse-product * / product? multiplier multiplicand)
    (list '/ simplify-debtor-op make-div inverse-div / * div? dividend divisor)
    (list '** simplify-unique-op make-exponentiation #f #f #f exponentiation? base exponent)
    (list '// simplify-unique-op make-log #f #f #f log? log-base log-val)))

(define simplification-rule-lookup (lookup-maker simplification-rules "simplification-rules"))

(define (op->simplification-rule s)
  (let* ((rule (simplification-rule-lookup s))
         (rule-op (car rule))
         (rule-args (cdr rule)))
    (lambda (a b)
      (apply rule-op (append (list a b) rule-args)))))

(define (simplify expr)
  (cond
    ((null? expr) expr)
    ((number? expr) expr)
    ((symbol? expr) expr)
    ((null? (cdr expr)) (simplify (car expr)))
    ((pair? expr) (let ((op (lowest-precedence-op expr)))
                   (let ((lhs (simplify (get-prefix op expr)))
                         (rhs (simplify (get-suffix op expr))))
                     ((op->simplification-rule op) lhs rhs))))
    (else expr)))
