(define constants-values
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

(define symbolic-constants (map car constants-values))

(define constant-lookup (lookup-maker constants-values "constants-values"))
(define (constant->value s) (simplify (constant-lookup s)))

(define (evaluate-constants expr)
  (cond
    ((constant-symbol? expr) (constant->value expr))
    ((pair? expr) (map evaluate-constants expr))
    (else expr)))

(define (constant-symbol? x)
  (and (symbol? x) (memq x symbolic-constants)))
