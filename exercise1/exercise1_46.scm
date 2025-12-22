(load "lib/math.scm")

(define (iterative-improve check-fn improve-fn)
  (lambda (guess)
    (define (try guess-n)
      (let ((next (improve-fn guess-n)))
        (if (check-fn guess-n next)
          next
          (try next))))
    (try guess)))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (let ((tolerance 0.00001))
      (< (abs (- v1 v2)) tolerance)))

  ((iterative-improve close-enough? f) first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (* y y) x)) newton-transform 1.0))

(display (sqrt 2.0)) ; => 1.4142135 ; proves iterative-improve works
