;;; This is assuming we have subtypes distinguishing between `real` and `integer`,
;;; similar to the `rectangular` and `polar` subtypes of `complex`
(put 'raise 'scheme-integer (lambda (i) (make-rational i 1)))
(put 'raise 'rational (lambda (r) (attach-tag 'scheme-real (/ (numer r) (denom r)))))
(put 'raise 'scheme-real (lambda (x) (make-complex-from-real-imag x 0)))

(define (raise x) (apply-generic 'raise x))
