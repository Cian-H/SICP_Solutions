(define supported-ops '(+ - * / ** //)) ; '// used as non-standard infix operator for log
(define (supported-op? op) (if (memq op supported-ops) #t #f))

(define op-precedence
  (list
    (list '+ 10)
    (list '- 10)
    (list '* 10)
    (list '/ 10)
    (list '** 10)
    (list '// 10)))

(define op-precedence-lookup (lookup-maker op-precedence "op-precedence"))
(define (op->precedence s) (car (op-precedence-lookup s)))

(define (op-precedence op)
  (cond ((memq op '(+ -)) 10) ; Lowest binding
    ((memq op '(* / //)) 20) ; Medium binding
    ((memq op '(**)) 30) ; Tightest binding
    (else 999)))

(define (lowest-precedence-op expr)
  (let* ((ops (filter supported-op? expr))
         (precs (map op-precedence ops))
         (min-prec (apply min precs)))
    (let ((sample-op (list-ref ops (list-index (lambda (p) (= p min-prec)) precs))))
      (if (right-associative? sample-op)
        (list-ref ops (list-index (lambda (p) (= p min-prec)) precs))
        (list-ref ops (list-index-last (lambda (p) (= p min-prec)) precs))))))

(define (create-op-predicate s) (lambda (x) (and (pair? x) (memq s x))))

; Basic predicates and selectors
(define sub? (create-op-predicate '-))
(define (minuend expr) (get-prefix '- expr))
(define (subtrahend expr) (simplify (get-suffix '- expr)))

(define sum? (create-op-predicate '+))
(define (addend expr) (get-prefix '+ expr))
(define (augend expr) (simplify (get-suffix '+ expr)))

(define div? (create-op-predicate '/))
(define (dividend expr) (get-prefix '/ expr))
(define (divisor expr) (simplify (get-suffix '/ expr)))

(define product? (create-op-predicate '*))
(define (multiplier expr) (get-prefix '* expr))
(define (multiplicand expr) (simplify (get-suffix '* expr)))

(define log? (create-op-predicate '//))
(define (log-base expr) (get-prefix '// expr))
(define (log-val expr) (simplify (get-suffix '// expr)))

(define exponentiation? (create-op-predicate '**))
(define (base expr) (get-prefix '** expr))
(define (exponent expr) (get-suffix '** expr))

;;; Inversions
(define (inverse-sum s)
  (cond
    ((number? s) (- s))
    ((and (pair? s) (eq? (cadr s) '-) (equal? (car s) 0)) (caddr s)) ;; Catch double negatives early here for simplicity
    (else (list 0 '- s))))

(define (inverse-sub s) s)

(define (inverse-product s)
  (if (number? s) (/ 1 s) (list 1 '/ s)))

(define (inverse-div s) s)
