(define (install-symbolic/constants-package)
  (register-type 'constant)

  (define *known-constants*
    (let* ((pi (acos -1.0))
           (e (exp 1.0))
           (sqrt2 (sqrt 2.0)))
      (list
        (list 'pi pi)
        (list 'tau (* 2 pi))
        (list 'deg-to-rad (/ pi 180.0))
        (list 'e e)
        (list 'log2e (/ 1 (log 2.0)))
        (list 'log10e (/ 1 (log 10.0)))
        (list 'sqrt2 sqrt2)
        (list 'sqrt1_2 (/ 1.0 sqrt2))
        (list 'phi (/ (+ 1.0 (sqrt 5.0)) 2.0))
        (list 'gamma 0.5772156649015329))))

  ;; Constructor: wraps the symbol and its value
  (define (make-constant sym)
    (let ((pair (assoc sym *known-constants*)))
      (if pair
        (type-wrap 'constant (list sym (cadr pair)))
        (error "Unknown constant: " sym))))

  ;; Expose a predicate for the parser
  (put 'predicate 'constant
    (lambda (sym) (if (assoc sym *known-constants*) #t #f)))

  (put 'make 'constant make-constant)
  'ok)
