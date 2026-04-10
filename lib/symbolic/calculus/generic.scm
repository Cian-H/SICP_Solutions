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

(define (integrate expr var)
  (cond
    ;; Integral of 0 is 0. Integral of c is c * x.
    ((number? expr) (if (zero? expr) 0 (mul expr var)))
    ((symbol? expr)
      (let ((constant-pred (get 'predicate 'constant)))
        (cond
          ;; Integral of a symbolic constant (like pi) is pi * x
          ((and constant-pred (constant-pred expr)) (mul ((get 'make 'constant) expr) var))
          ;; Integral of the variable itself (x) is (x^2) / 2
          ((eq? expr var) (div (type-wrap '** (list var 2)) 2))
          ;; Integral of a different variable (y w.r.t x) is y * x
          (else (mul expr var)))))
    (else
      (let* ((expr-type (type-of expr))
             (integrate-proc (get 'integrate expr-type)))
        (if integrate-proc
          (integrate-proc (type-unwrap expr) var)
          ;; If we don't know how to integrate it, return an unevaluated integral form
          (list 'integral expr 'd var))))))
