(define (deriv-exponential-power-rule u v var)
  (make-product
    (make-product v (make-exponentiation u (- v 1)))
    (deriv u var)))

(define (deriv-exponential-exponential-rule expr v var)
  (make-product expr (deriv v var)))

(define (deriv-exponential-constbase-rule expr u v var)
  (make-product
    (make-product expr (make-ln u))
    (deriv v var)))

(define (deriv-exponential-general u v var)
  (make-product
    (make-exponentiation u v)
    (make-sum
      (make-product (deriv v var) (make-ln u))
      (make-product v (make-div (deriv u var) u)))))

;;; Main derivative procedure
(define (deriv expr var)
  (cond
    ((number? expr) 0)
    ((variable? expr) (if (same-variable? expr var) 1 0))
    ((constant-symbol? expr) 0)

    ((pair? expr) (let* ((op (lowest-precedence-op expr))
                         (u (get-prefix op expr))
                         (v (get-suffix op expr)))
                   (cond
                     ((eq? op '+) (make-sum (deriv u var) (deriv v var)))
                     ((eq? op '-) (make-sub (deriv u var) (deriv v var)))
                     ((eq? op '*) (make-sum (make-product u (deriv v var))
                                   (make-product (deriv u var) v)))
                     ((eq? op '/) (make-div (make-sub (make-product v (deriv u var))
                                             (make-product u (deriv v var)))
                                   (make-exponentiation v 2)))
                     ((eq? op '**) (let ((base u) (exponent v))
                                    (cond ((number? exponent) (deriv-exponential-power-rule base exponent var))
                                      ((eq? base 'e) (deriv-exponential-exponential-rule expr exponent var))
                                      (else (deriv-exponential-general base exponent var)))))
                     ((eq? op '//) (cond
                                    ((or (number? u) (constant-symbol? u)) (make-div (deriv v var) (make-product v (make-ln u))))
                                    (else (deriv (make-div (make-ln v) (make-ln u)) var))))
                     (else (error "Unknown operator:" op)))))
    (else (error "Unknown expression type" expr))))
