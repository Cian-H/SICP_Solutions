(define (install-exp-package)
  (if (not (get 'deriv 'ln))
    (safe-load "lib/symbolic/calculus/ln.scm"))
  (install-ln-package)

  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))

  (define (make-exp x y)
    (cond ((and (number? x) (number? y)) (expt x y))
      ((eq? y 0) 1)
      ((eq? y 1) x)
      ((eq? x 0) 0)
      ((eq? x 1) 1)
      (else (type-wrap '** (list x y)))))

  (define (deriv-exp operands var)
    (let ((u (base operands))
          (v (exponent operands)))
      (cond
        ((number? v)
          ;; Power rule: v * u^(v-1) * du/dx
          (mul (mul v (make-exp u (- v 1)))
            (deriv u var)))
        ((eq? u 'e)
          ;; Exponential rule: e^v * dv/dx
          (mul (make-exp u v) (deriv v var)))
        (else
          ;; General rule: u^v * (dv/dx * ln(u) + v/u * du/dx)
          (mul (make-exp u v)
            (add (mul (deriv v var) (ln u))
              (mul v (div (deriv u var) u))))))))

  (put 'deriv '** deriv-exp)
  (put 'exp '** make-exp)
  'ok)
