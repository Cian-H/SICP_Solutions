(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (is-close? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond
          ((positive? test-value) (search f neg-point midpoint))
          ((negative? test-value) (search f midpoint pos-point))
          (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond
      ((and (negative? a-value) (positive? b-value)) (search f a b))
      ((and (negative? b-value) (positive? a-value)) (search f b a))
      (else (error "Values are not of opposite sign" a b)))))

(define (iterative-improve check-fn improve-fn)
  (lambda (guess)
    (define (try guess-n)
      (let ((next (improve-fn guess-n)))
        (if (check-fn guess-n next)
          next
          (try next))))
    (try guess)))

(define (fixed-point f first-guess)
  ((iterative-improve is-close? f) first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
