(define (make-rat n d)
  (let ((n_norm (if (negative? d) (- n) n))
        (g (gcd n_norm d)))
    (cons (/ n_norm g) (/ (abs d) g))))
